{-# LANGUAGE Trustworthy, FlexibleContexts, UndecidableInstances, PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{- |
    License     :  BSD-style
    Module      :  Data.Field
    Copyright   :  (c) Andrey Mulik 2020
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.Field@ provides fake field type for record-style operations.
-}
module Data.Field
(
  -- * Field
  pattern SField, Field (..), ThisProp (..),
  
  -- ** Observable field
  Observe (..), OField, observe,
)
where

import Data.Property
import Data.Typeable
import Data.Functor
import Data.IORef
import Data.STRef

import GHC.Conc

import Control.Concurrent.MVar
import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | Normal field, which contain getter, setter and modifier.
data Field m record a = Field
  {
    -- | Get field value
    getField    :: !(record -> m a),
    -- | Set field value
    setField    :: !(record -> a -> m ()),
    -- | Modify field value
    modifyField :: !(record -> (a -> a) -> m a)
  }
  deriving ( Typeable )

instance GetProp    Field record where getRecord    = getField
instance SetProp    Field record where setRecord    = setField
instance ModifyProp Field record where modifyRecord = modifyField

instance (Integral switch) => SwitchProp Field switch
  where
    switchRecord field record = void . modifyRecord field record . (+) . fromIntegral

instance {-# INCOHERENT #-} SwitchProp Field Bool
  where
    switchRecord record field n = void $ modifyRecord record field (even n &&)

--------------------------------------------------------------------------------

pattern SField :: (Monad m) => (record -> m a) -> (record -> a -> m ()) -> Field m record a
pattern SField g s <- Field g s _ where SField g s = Field g s (modifyDummy g s)

modifyDummy :: (Monad m) => (record -> m a) -> (record -> a -> m ()) -> (record -> (a -> a) -> m a)
modifyDummy g s = \ record f -> do val <- f <$> g record; s record val; return val

--------------------------------------------------------------------------------

-- | Simple field observer, which can run some handlers after each action.
data Observe field m record a = Observe
  {
    -- | Field to observe.
    observed :: field m record a,
    -- | 'getRecord' observer
    onGet    :: record -> a -> m (),
    -- | 'setRecord' observer
    onSet    :: record -> a -> m (),
    -- | 'modifyRecord' observer
    onModify :: record -> m ()
  }
  deriving ( Typeable )

-- | Observable 'Field'.
type OField = Observe Field

-- | Create field with default observers.
observe :: (Monad m) => field m record a -> Observe field m record a
observe field =
  let og = \ _ _ -> return (); om = \ _ -> return (); os = og
  in  Observe field og os om

instance (SwitchProp field a) => SwitchProp (Observe field) a
  where
    switchRecord field record n = do
      switchRecord (observed field) record n
      onModify field record

instance (GetProp field record) => GetProp (Observe field) record
  where
    getRecord field record = do
      res <- getRecord (observed field) record
      onGet field record res
      return res

instance (SetProp field record) => SetProp (Observe field) record
  where
    setRecord field record val = do
      setRecord (observed field) record val
      onSet field record val

instance (ModifyProp field record) => ModifyProp (Observe field) record
  where
    modifyRecord field record upd = do
      res <- modifyRecord (observed field) record upd
      onModify field record
      return res

--------------------------------------------------------------------------------

{- |
  The 'ThisProp' class provides 'this' field for entire record access. It is
  supposed to be useful for working with mutable variables and structures when
  @(':~')@ is difficult or impairs the readability of the code, e.g:
  
  > set record [this := value] == set record [anyField :~ const value]
-}
class (Monad m) => ThisProp m record this | record -> m, record -> this
  where
    -- | Record accessor.
    this :: Field m record this

instance ThisProp IO (MVar a) a where this = Field readMVar putMVar modifyMVar'

modifyMVar' :: MVar a -> (a -> a) -> IO a
modifyMVar' mvar f = mvar `modifyMVar` \ a -> let b = f a in return (b, b)

instance ThisProp STM (TVar a) a where this = Field readTVar writeTVar modifyTVar

modifyTVar :: TVar a -> (a -> a) -> STM a
modifyTVar var f = do res <- f <$> readTVar var; writeTVar var res; return res

instance ThisProp IO (IORef a) a where this = Field readIORef writeIORef modifyIORef''

modifyIORef'' :: IORef a -> (a -> a) -> IO a
modifyIORef'' var f = var `atomicModifyIORef'` \ a -> let b = f a in (b, b)

instance ThisProp (ST s) (STRef s a) a where this = Field readSTRef writeSTRef modifySTRef''

modifySTRef'' :: STRef s a -> (a -> a) -> ST s a
modifySTRef'' var f = do res <- f <$> readSTRef var; writeSTRef var res; return $! res



