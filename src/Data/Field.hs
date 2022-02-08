{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Trustworthy, PatternSynonyms, ViewPatterns, DefaultSignatures #-}
{-# LANGUAGE UndecidableSuperClasses, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}

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
  Field, GField, DField, DMField, sfield,
  
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

-- | Normal field, which contain getter, setter and modifiers (pure and monadic).
type Field m record e = GField m record e [FieldGetA, FieldSetA, FieldModifyA, FieldModifyMA]

{- |
  @since 0.3
  
  Generalized field with 'FieldC' convention.
-}
type GField m record e = FObject (FieldC m record e)

{- |
  @since 0.3
  
  Dummy field type, pure accessor to variable.
-}
type DField var record e = record -> var e

{- |
  @since 0.3
  
  Dummy field type, monadic accessor to variable.
-}
type DMField m var record e = record -> m (var e)

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
    this =  subfield id
    
    -- | 'subfield' is common variable access field.
    default subfield :: Default
      (
        DefaultField (DField var record e) (FieldC m record e)
          [FieldGetA, FieldSetA, FieldModifyA, FieldModifyMA]
      ) => DField var record e -> Field m record e
    
    subfield :: DField var record e -> Field m record e
    subfield =  defaultField
    
    -- | 'subfieldM' is mutable field variable accessor.
    default subfieldM :: Default
      (
        DefaultField (DMField m var record e) (FieldC m record e)
          [FieldGetA, FieldSetA, FieldModifyA, FieldModifyMA]
      ) => DMField m var record e -> Field m record e
    
    subfieldM :: DMField m var record e -> Field m record e
    subfieldM =  defaultField
    
    -- | Create and initialize new mutable variable.
    var :: e -> m (var e)

instance IsMVar (ST s) (STRef s) where var = newSTRef
instance IsMVar IO     IORef     where var = newIORef
instance IsMVar IO     MVar      where var = newMVar
instance IsMVar STM    TVar      where var = newTVar

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

