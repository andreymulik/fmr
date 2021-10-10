{-# LANGUAGE UndecidableSuperClasses, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE Trustworthy, TypeFamilies, FlexibleContexts, PatternSynonyms #-}

{- |
    License     :  BSD-style
    Module      :  Data.Field
    Copyright   :  (c) Andrey Mulik 2020-2021
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.Field@ provides immutable field type for record-style operations.
-}
module Data.Field
(
  -- * Field
  Field (..),
  GetterFor, SetterFor, ModifierFor, ModifierMFor,
  
  -- * IsMVar and MonadVar
  IsMVar (..), MonadVar (..)
)
where

import Prelude hiding ( (.), id )
import Data.Property
import Data.Typeable
import Data.Functor
import Data.IORef
import Data.STRef
import Data.Kind

import GHC.Conc

import Control.Concurrent.MVar
import Control.Category
import Control.Monad.ST
import Control.Monad

default ()

--------------------------------------------------------------------------------

{- |
  Normal field, which contain getter, setter and modifier.
  
  Since @fmr-0.2@, you can also combine fmr fields using @('.')@ and @'id'@ from
  the 'Category' class:
  
  @
    outer :: (Monad m) => Field m outer inner
    inner :: (Monad m) => Field m inner value
    
    field :: (Monad m) => Field m outer value
    field =  outer.inner
  @
-}
data Field m record a = Field
  {
    -- | Field getter
    getField :: !(GetterFor m record a),
    -- | Field setter
    setField :: !(SetterFor m record a),
    -- | Field modifier
    modifyField :: !(ModifierFor m record a),
    -- | Monadic field modifier
    modifyFieldM :: !(ModifierMFor m record a)
  } deriving ( Typeable )

-- | Getter type.
type GetterFor m record a = record -> m a

-- | Setter type.
type SetterFor m record a = record -> a -> m ()

-- | Modifier type.
type ModifierFor  m record a = record -> (a -> a) -> m a

-- | Monadic modifier type.
type ModifierMFor m record a = record -> (a -> m a) -> m a

--------------------------------------------------------------------------------

instance (Monad m) => Category (Field m)
  where
    Field g1 s1 m1 mm1 . Field g2 _ _ _ = Field (g1 <=< g2) s3 m3 mm3
      where
        mm3 record   go  = flip mm1  go  =<< g2 record
        m3  record   f   = flip m1   f   =<< g2 record
        s3  record value = flip s1 value =<< g2 record
    
    id = Field return (\ _ _ -> return ()) (\ x f -> return (f x)) (flip ($))

--------------------------------------------------------------------------------

instance FieldGet    Field where getRecord    = getField
instance FieldSet    Field where setRecord    = setField
instance FieldModify Field where modifyRecord = modifyField
instance FieldSwitch Field
  where
    switchRecord field record = void .
      modifyRecord field record . toggle

--------------------------------------------------------------------------------

{- |
  The 'IsMVar' class provides 'this' field for entire record access.
  
  Please note that you cannot create 'IsMVar' and 'MonadVar' instances for some
  monad separately.
-}
class (Monad m, MonadVar m) => IsMVar m var
  where
    -- | 'this' is common variable access field.
    this :: Field m (var a) a
    
    -- | Create and initialize new mutable variable.
    var :: a -> m (var a)

instance IsMVar (ST s) (STRef s)
  where
    var  = newSTRef
    this = Field readSTRef writeSTRef modify' modifyM'
      where
        modifyM' ref f = do res <- f =<< readSTRef ref; res <$ writeSTRef ref res
        modify'  ref f = do res <- f <$> readSTRef ref; res <$ writeSTRef ref res

instance IsMVar IO IORef
  where
    var  = newIORef
    this = Field readIORef writeIORef modify' modifyM'
      where
        modifyM' ref f = do val <- f =<< readIORef ref; writeIORef ref val; return val
        modify'  ref f = atomicModifyIORef' ref (\ a -> let b = f a in (b, b))

instance IsMVar IO MVar
  where
    var  = newMVar
    this = Field readMVar putMVar modify' modifyM'
      where
        modifyM' mvar f = mvar `modifyMVarMasked` \ a -> do b <- f a; return (b, b)
        modify'  mvar f = mvar `modifyMVar` \ a -> let b = f a in return (b, b)

instance IsMVar STM TVar
  where
    var  = newTVar
    this = Field readTVar writeTVar modifyTVar modifyMTVar
      where
        modifyMTVar tvar f = do res <- f =<< readTVar tvar; res <$ writeTVar tvar res
        modifyTVar  tvar f = do res <- f <$> readTVar tvar; res <$ writeTVar tvar res

--------------------------------------------------------------------------------

{- |
  'MonadVar' is a class of monads for which defined at least one type of mutable
  variable.
  
  Note that a variable of type @(Var m)@ should be as simple possible for a
  given monad. I only has to implement the basic operations triad: read,
  write and update (which don't have to be atomic).
-}
class (Monad m, IsMVar m (Var m)) => MonadVar m
  where
    -- | @('Var' m)@ is type of mutable variable in monad @m@.
    type Var m :: Type -> Type

instance MonadVar (ST s) where type Var (ST s) = STRef s
instance MonadVar IO     where type Var IO     = IORef
instance MonadVar STM    where type Var STM    = TVar



