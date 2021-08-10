{-# LANGUAGE UndecidableSuperClasses, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE Trustworthy, TypeFamilies, FlexibleContexts, PatternSynonyms #-}

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
  Field (..), GetterFor, SetterFor, ModifierFor, pattern SField,
  
  -- * IsMVar and MonadVar
  IsMVar (..), MonadVar (..),
  
  -- ** Observable field
  Observe (..), OField, observe,
)
where

import Data.Property
import Data.Typeable
import Data.Functor
import Data.IORef
import Data.STRef
import Data.Kind

import GHC.Conc

import Control.Concurrent.MVar
import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | Normal field, which contain getter, setter and modifier.
data Field m record a = Field
  {
    -- | Get field value
    getField    :: !(GetterFor   m record a),
    -- | Set field value
    setField    :: !(SetterFor   m record a),
    -- | Modify field value
    modifyField :: !(ModifierFor m record a)
  }
  deriving ( Typeable )

-- | Getter type.
type GetterFor   m record a = record -> m a

-- | Setter type.
type SetterFor   m record a = record -> a -> m ()

-- | Modifier type.
type ModifierFor m record a = record -> (a -> a) -> m a

-- | 'SField' pattern simplifies field creation if modifier uses getter and setter.
pattern SField :: (Monad m) => GetterFor m record a -> SetterFor m record a -> Field m record a
pattern SField g s <- Field g s _ where SField g s = Field g s (modifyDummy g s)

modifyDummy :: (Monad m) => GetterFor m record a -> SetterFor m record a -> ModifierFor m record a
modifyDummy g s = \ record f -> do val <- f <$> g record; s record val; return val

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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
  'MonadVar' is a class of monads for which defined at least one type of mutable
  variable.
  
  Note that a variable of type @(Var m)@ should be as simple possible for a
  given monad. It doesn't have high requirements (e.g. thread safety), it only
  has to implement the basic triad of operations: read, write and update (which
  don't have to be atomic).
-}
class (Monad m, IsMVar m (Var m)) => MonadVar m
  where
    -- | @('Var' m)@ is type of mutable variable in monad @m@.
    type Var m :: Type -> Type

instance MonadVar (ST s) where type Var (ST s) = STRef s
instance MonadVar STM    where type Var STM    = TVar
instance MonadVar IO     where type Var IO     = IORef

--------------------------------------------------------------------------------

{- |
  The 'IsMVar' class provides 'this' field for entire record access.
  
  It's supposed to be useful for working with mutable variables and structures
  when @(':~')@ is difficult or impairs the readability of the code, e.g:
  
  > set record [this := value] === set record [anyField :~ const value]
  
  Please note that you cannot create 'IsMVar' and 'MonadVar' instances for some
  monad separately: if a mutable variable is defined in the monad, then there
  must be a default variable type (not necessarily which one), and if there is a
  default variable type, then there must be at least one such variable.
-}
class (Monad m, MonadVar m) => IsMVar m var
  where
    -- | 'this' is common variable access field.
    this :: Field m (var this) this

instance IsMVar (ST s) (STRef s) where this = Field readSTRef writeSTRef modifySTRef''

modifySTRef'' :: STRef s a -> (a -> a) -> ST s a
modifySTRef'' var f = do res <- f <$> readSTRef var; writeSTRef var res; return $! res

instance IsMVar IO IORef where this = Field readIORef writeIORef modifyIORef''

modifyIORef'' :: IORef a -> (a -> a) -> IO a
modifyIORef'' var f = var `atomicModifyIORef'` \ a -> let b = f a in (b, b)

instance IsMVar IO MVar where this = Field readMVar putMVar modifyMVar'

modifyMVar' :: MVar a -> (a -> a) -> IO a
modifyMVar' mvar f = mvar `modifyMVar` \ a -> let b = f a in return (b, b)

instance IsMVar STM TVar where this = Field readTVar writeTVar modifyTVar

modifyTVar :: TVar a -> (a -> a) -> STM a
modifyTVar var f = do res <- f <$> readTVar var; writeTVar var res; return res

