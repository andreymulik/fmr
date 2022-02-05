{-# LANGUAGE Trustworthy, FlexibleContexts, UndecidableSuperClasses, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, TypeFamilies, TypeOperators #-}

{- |
    License     :  BSD-style
    Module      :  Data.Field
    Copyright   :  (c) Andrey Mulik 2020-2022
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.Field@ provides immutable field type for record-style operations.
-}
module Data.Field
(
  -- * Exports
  module Data.Field.Object,
  module Data.Property,
  
  -- * Field
  FObject ( Field, getField, setField, modifyField, modifyFieldM ),
  Field, GField, sfield,
  
  -- * IsMVar and MonadVar
  IsMVar (..), MonadVar (..)
)
where

import Data.Field.Object
import Data.Property
import Data.IORef
import Data.STRef
import Data.Kind

import GHC.Conc

import Control.Concurrent.MVar
import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

type GField m record e = FObject (FieldC m record e)

-- | Normal field, which contain getter, setter and modifier.
type Field m record e = GField m record e [FieldGetA, FieldSetA, FieldModifyA, FieldModifyMA]

--------------------------------------------------------------------------------

{-# COMPLETE Field #-}

pattern Field :: GetterFor   m record e -> SetterFor    m record e
              -> ModifierFor m record e -> ModifierMFor m record e
              -> Field m record e
pattern Field{getField, setField, modifyField, modifyFieldM} <-
    (
      (\ f -> (getRecord f, setRecord f, modifyRecord f, modifyRecordM f)) ->
      (getField, setField, modifyField, modifyFieldM)
    )
  where
    Field g s m mm = FieldGetA      g :++ FieldSetA  s :++ FieldModifyA m :++
                     FieldModifyMA mm :++ FObjectEmpty

-- | 'sfield' creates new field from given getter and setter.
sfield :: Monad m => GetterFor m record e -> SetterFor m record e -> Field m record e
sfield g s = Field g s
  (\ record  f -> do res <-  f <$> g record; s record res; return res)
  (\ record go -> do res <- go =<< g record; s record res; return res)

--------------------------------------------------------------------------------

{- |
  The 'IsMVar' class provides 'this' field for entire record access.
  
  Please note that you cannot create 'IsMVar' and 'MonadVar' instances for some
  monad separately.
-}
class MonadVar m => IsMVar m var | var -> m
  where
    -- | 'this' is common variable access field.
    this :: Field m (var e) e
    
    -- | Create and initialize new mutable variable.
    var :: e -> m (var e)

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


