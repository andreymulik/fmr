{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE Trustworthy, PatternSynonyms, ViewPatterns, DefaultSignatures #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds #-}

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
  module Data.Property,
  
  -- * Field
  GField ( .., SField, Field, getField, setField, modifyField, modifyFieldM ),
  FieldA, Field, sfield, this',
  
  -- * IsMVar and MonadVar
  IsMVar (..), MonadVar (..)
)
where

import Data.Field.Object
import Data.Typeable
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
type Field = GField FieldA FieldC

{- |
  @since 0.3
  
  Generalized field with 'FieldC' convention.
-}
newtype GField as fieldC (m :: Type -> Type) record e = GField
  {
    fromGField :: FObject as (fieldC m record e)
  } deriving ( Typeable )

-- | Default list of accessors.
type FieldA = [FieldGetA, FieldSetA, FieldModifyA, FieldModifyMA]

--------------------------------------------------------------------------------

{-# COMPLETE Field #-}

-- | Create Field from getter, setter and modifiers.
pattern Field :: GetterFor   m record e -> SetterFor    m record e
              -> ModifierFor m record e -> ModifierMFor m record e
              -> Field m record e
pattern Field{getField, setField, modifyField, modifyFieldM} <-
    (
      (\ (GField f) -> (getRecord f, setRecord f, modifyRecord f, modifyRecordM f))
                    -> (getField, setField, modifyField, modifyFieldM)
    )
  where
    Field g s m mm = GField
      (
        FieldGetA    g :++ FieldSetA      s :++
        FieldModifyA m :++ FieldModifyMA mm :++ FObjectEmpty
      )

{- |
  @since 0.3
  
  Good old pattern from @fmr-0.1@, simplified version of 'Field'.
-}
pattern SField :: Monad m => GetterFor m record e -> SetterFor m record e -> Field m record e
pattern SField g s <- ((\ (GField f) -> (getRecord f, setRecord f)) -> (g, s))
  where
    SField = sfield

instance IsField (FObject as (f m record e)) c a
      => IsField  (GField as  f m record e)  c a
  where
    fromField = fromField.fromGField

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
        DefaultField (SubField var record e) (FieldC m record e) FieldA
      ) => SubField var record e -> Field m record e
    
    subfield :: SubField var record e -> Field m record e
    subfield =  GField . defaultField
    
    -- | 'subfieldM' is mutable field variable accessor.
    default subfieldM :: Default
      (DefaultField (SubFieldM m var record e) (FieldC m record e) FieldA) =>
      SubFieldM m var record e -> Field m record e
    
    subfieldM :: SubFieldM m var record e -> Field m record e
    subfieldM =  GField . defaultField
    
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

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Same as 'this', but have more strict type.
-}
this' :: MonadVar m => Field m (Var m e) e
this' =  this



