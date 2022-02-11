{-# LANGUAGE Trustworthy, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TypeOperators, TypeFamilies, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GADTs, ConstraintKinds, DataKinds #-}

{- |
    License     :  BSD-style
    Module      :  Data.Field.Object
    Copyright   :  (c) Andrey Mulik 2022
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.Field.Object@ provides generalized type family restricted field.
-}
module Data.Field.Object
(
  -- * Generalized field type
  FObject ( .., FObjectElem ), IsField (..),
  type (~:=), type (~<=), type (~</=),
  
  -- ** Default field
  module Data.Default.Class, DefaultField (..), defaultField,
  
  -- * Field convention
  FieldC (..), FieldGetA, FieldSetA, FieldModifyA, FieldModifyMA,
  GetterFor, SetterFor, ModifierFor, ModifierMFor
)
where

import Data.Default.Class
import Data.IORef
import Data.STRef
import Data.Kind

import GHC.TypeLits
import GHC.Conc

import Control.Concurrent.MVar
import Control.Monad.ST
import Control.Monad

default ()

infixr 6 :++

--------------------------------------------------------------------------------

{- |
  @'FObject' c as@ is generalized field representation, where @c@ is convention
  type operation (usually, type family), @as@ - list of stored (supported)
  accessors.
-}
data FObject c (as :: [Symbol])
  where
    FObjectEmpty :: FObject c '[]
    (:++)        :: a ~</= as => c a -> FObject c as -> FObject c (a : as)

{-# COMPLETE FObjectElem #-}

-- | Extract accessor from field, completely useless in older GHC (too generic).
pattern FObjectElem :: a ~:= as => c a -> FObject c as
pattern FObjectElem x <- (getFObject -> x)

--------------------------------------------------------------------------------

{- |
  The @'IsField' field@ class is designed to extract accessors from @field@.
  Normally, accessors are wrapped in data or newtype, so for a more comfortable
  work with fields, it is recommended to use helper functions like
  'Data.Property.getRecord'.
-}
class IsField field c (a :: Symbol) | field -> c
  where
    type a ~?= field :: Constraint
    type a ~?= field =  'True ~ 'True
    
    -- Get accessor from generalized field.
    fromField :: a ~?= field => field -> c a

instance IsField (FObject c as) c a
  where
    type a ~?= FObject c as = a ~:= as
    
    fromField = getFObject

instance Default (FObject c '[])
  where
    def = FObjectEmpty

instance Default (DefaultField field c '[])
  where
    def = DefaultField def

-- | Newtype wrapper for @(field -> 'FObject' c as)@ functions.
newtype DefaultField field c as = DefaultField
  {fromDefaultField :: field -> FObject c as}

instance
    (
      a ~</= as, a ~?= field, IsField field c a, Default (DefaultField field c as)
    ) => Default (DefaultField field c (a : as))
  where
    def = DefaultField (\ field -> fromField field :++ fromDefaultField def field)

{- |
  Convert some field to @'FObject'@.
  May reduce the variety of available accessors.
-}
defaultField :: Default (DefaultField field c as) => field -> FObject c as
defaultField =  fromDefaultField def

--------------------------------------------------------------------------------

{- STRef instances. -}

instance IsField (record -> STRef s e) (FieldC (ST s) record e) FieldGetA
  where
    fromField field = FieldGetA (readSTRef.field)

instance IsField (record -> STRef s e) (FieldC (ST s) record e) FieldSetA
  where
    fromField field = FieldSetA (writeSTRef.field)

instance IsField (record -> STRef s e) (FieldC (ST s) record e) FieldModifyA
  where
    fromField field = FieldModifyA $ \ record f -> let ref = field record in do
      res <- f <$> readSTRef ref
      res <$ writeSTRef ref res

instance IsField (record -> STRef s e) (FieldC (ST s) record e) FieldModifyMA
  where
    fromField field = FieldModifyMA $ \ record f -> let ref = field record in do
      res <- f =<< readSTRef ref
      res <$ writeSTRef ref res

instance IsField (record -> ST s (STRef s e)) (FieldC (ST s) record e) FieldGetA
  where
    fromField field = FieldGetA (readSTRef <=< field)

instance IsField (record -> ST s (STRef s e)) (FieldC (ST s) record e) FieldSetA
  where
    fromField field = FieldSetA (writeSTRef <<=< field)

instance IsField (record -> ST s (STRef s e)) (FieldC (ST s) record e) FieldModifyA
  where
    fromField field = FieldModifyA $ \ record f -> do
      ref <- field record
      res <- f <$> readSTRef ref
      res <$ writeSTRef ref res

instance IsField (record -> ST s (STRef s e)) (FieldC (ST s) record e) FieldModifyMA
  where
    fromField field = FieldModifyMA $ \ record f -> do
      ref <- field record
      res <- f =<< readSTRef ref
      res <$ writeSTRef ref res

--------------------------------------------------------------------------------

{- IORef instances. -}

instance IsField (record -> IORef e) (FieldC IO record e) FieldGetA
  where
    fromField field = FieldGetA (readIORef.field)

instance IsField (record -> IORef e) (FieldC IO record e) FieldSetA
  where
    fromField field = FieldSetA (writeIORef.field)

instance IsField (record -> IORef e) (FieldC IO record e) FieldModifyA
  where
    fromField field = FieldModifyA $ \ record f ->
      field record `atomicModifyIORef'` \ a -> let b = f a in (b, b)

instance IsField (record -> IORef e) (FieldC IO record e) FieldModifyMA
  where
    fromField field = FieldModifyMA $ \ record f -> let ref = field record in do
      val <- f =<< readIORef ref
      val <$ writeIORef ref val

instance IsField (record -> IO (IORef e)) (FieldC IO record e) FieldGetA
  where
    fromField field = FieldGetA (readIORef <=< field)

instance IsField (record -> IO (IORef e)) (FieldC IO record e) FieldSetA
  where
    fromField field = FieldSetA (writeIORef <<=< field)

instance IsField (record -> IO (IORef e)) (FieldC IO record e) FieldModifyA
  where
    fromField field = FieldModifyA $ \ record f -> do
      ref <- field record
      ref `atomicModifyIORef'` \ a -> let b = f a in (b, b)

instance IsField (record -> IO (IORef e)) (FieldC IO record e) FieldModifyMA
  where
    fromField field = FieldModifyMA $ \ record f -> do
      ref <- field record
      val <- f =<< readIORef ref
      val <$ writeIORef ref val

--------------------------------------------------------------------------------

{- MVar instances. -}

instance IsField (record -> MVar e) (FieldC IO record e) FieldGetA
  where
    fromField field = FieldGetA (readMVar.field)

instance IsField (record -> MVar e) (FieldC IO record e) FieldSetA
  where
    fromField field = FieldSetA (putMVar.field)

instance IsField (record -> MVar e) (FieldC IO record e) FieldModifyA
  where
    fromField field = FieldModifyA $ \ record f ->
      field record `modifyMVar` \ a -> let b = f a in return (b, b)

instance IsField (record -> MVar e) (FieldC IO record e) FieldModifyMA
  where
    fromField field = FieldModifyMA $ \ record f ->
      field record `modifyMVarMasked` \ a -> do b <- f a; return (b, b)

instance IsField (record -> IO (MVar e)) (FieldC IO record e) FieldGetA
  where
    fromField field = FieldGetA (readMVar <=< field)

instance IsField (record -> IO (MVar e)) (FieldC IO record e) FieldSetA
  where
    fromField field = FieldSetA (putMVar <<=< field)

instance IsField (record -> IO (MVar e)) (FieldC IO record e) FieldModifyA
  where
    fromField field = FieldModifyA $ \ record f -> do
      mvar <- field record
      mvar `modifyMVar` \ a -> let b = f a in return (b, b)

instance IsField (record -> IO (MVar e)) (FieldC IO record e) FieldModifyMA
  where
    fromField field = FieldModifyMA $ \ record f -> do
      mvar <- field record
      mvar `modifyMVarMasked` \ a -> do b <- f a; return (b, b)

--------------------------------------------------------------------------------

{- TVar instances. -}

instance IsField (record -> TVar e) (FieldC STM record e) FieldGetA
  where
    fromField field = FieldGetA (readTVar.field)

instance IsField (record -> TVar e) (FieldC STM record e) FieldSetA
  where
    fromField field = FieldSetA (writeTVar.field)

instance IsField (record -> TVar e) (FieldC STM record e) FieldModifyA
  where
    fromField field = FieldModifyA $ \ record f -> let tvar = field record in do
      res <- f <$> readTVar tvar
      res <$ writeTVar tvar res

instance IsField (record -> TVar e) (FieldC STM record e) FieldModifyMA
  where
    fromField field = FieldModifyMA $ \ record f -> let tvar = field record in do
      res <- f =<< readTVar tvar
      res <$ writeTVar tvar res

instance IsField (record -> STM (TVar e)) (FieldC STM record e) FieldGetA
  where
    fromField field = FieldGetA (readTVar <=< field)

instance IsField (record -> STM (TVar e)) (FieldC STM record e) FieldSetA
  where
    fromField field = FieldSetA (writeTVar <<=< field)

instance IsField (record -> STM (TVar e)) (FieldC STM record e) FieldModifyA
  where
    fromField field = FieldModifyA $ \ record f -> do
      tvar <- field record
      res  <- f <$> readTVar tvar
      res  <$ writeTVar tvar res

instance IsField (record -> STM (TVar e)) (FieldC STM record e) FieldModifyMA
  where
    fromField field = FieldModifyMA $ \ record f -> do
      tvar <- field record
      res  <- f =<< readTVar tvar
      res  <$ writeTVar tvar res

--------------------------------------------------------------------------------

{- |
  'FieldC' is the main data access type family used by [G]Field and [G]MField.
  
  * To avoid possible version compatibility issues, it is recommended to use
  type synonyms ('FieldGetA', 'FieldSetA', etc.), since types of kind 'Symbol'
  are, in essence, magic strings.
  * To avoid collisions between extension libraries, it is recommended to use
  'Symbol's of the form @"lib-name.basic-convention.accessor-name"@ as the
  symbolic parameter of the convention.
  * If you are using a third party library convention (without including it as a
  dependency), it is recommended to create a type synonym for it rather than
  using it directly.
-}
data family FieldC (m :: Type -> Type) record e (c :: Symbol) :: Type

--------------------------------------------------------------------------------

-- | Common get accessor.
newtype instance FieldC m record e FieldGetA = FieldGetA
  {fieldGetA :: GetterFor m record e}

-- | Common get accessor convention.
type FieldGetA = "fmr.field.get"

-- | Common get accessor type.
type GetterFor m record e = record -> m e

--------------------------------------------------------------------------------

-- | Common set accessor.
newtype instance FieldC m record e FieldSetA = FieldSetA
  {fieldSetA :: SetterFor m record e}

-- | Common set accessor convention.
type FieldSetA = "fmr.field.set"

-- | Common set accessor type.
type SetterFor m record e = record -> e -> m ()

--------------------------------------------------------------------------------

-- | Common modify accessor.
newtype instance FieldC m record e FieldModifyA = FieldModifyA
  {fieldModifyA :: ModifierFor m record e}

-- | Common modify accessor convention.
type FieldModifyA = "fmr.field.modify"

-- | Common modify accessor type.
type ModifierFor m record e = record -> (e -> e) -> m e

--------------------------------------------------------------------------------

-- | Common monadic modify accessor.
newtype instance FieldC m record e FieldModifyMA = FieldModifyMA
  {fieldModifyMA :: ModifierMFor m record e}

-- | Common monadic modify accessor convention.
type FieldModifyMA = "fmr.field.modifyM"

-- | Common monadic modify accessor type.
type ModifierMFor m record e = record -> (e -> m e) -> m e

--------------------------------------------------------------------------------

{- |
  Internal type family, defines whether an accessor is included to given field
  accessors list.
-}
type family FObjectElem (c :: Symbol) cs
  where
    FObjectElem _   '[ ]   = 'False
    FObjectElem c (c :  _) = 'True
    FObjectElem c (_ : cs) = FObjectElem c cs

-- | @c ~<= cs@ means that @c@ is included to the @cs@ field accessors list.
type c ~<= cs = FObjectElem c cs ~ 'True

-- | @c ~<= cs@ means that @c@ isn't included to the @cs@ field accessors list.
type c ~</= cs = FObjectElem c cs ~ 'False

--------------------------------------------------------------------------------

-- | Closed class of generalized field accessor getters.
class a ~<= as => a ~:= as
  where
    getFObject :: FObject c as -> c a

instance {-# INCOHERENT #-} a ~:= (a : as) where getFObject (a :++ _) = a

instance {-# INCOHERENT #-} (a ~<= (a' : as), a ~:= as) => a ~:= (a' : as)
  where
    getFObject (_ :++ as) = getFObject as

--------------------------------------------------------------------------------

-- | Monadic version of @('...')@.
(<<=<) :: Monad m => (c -> b -> m d) -> (a -> m c) -> a -> b -> m d
(<<=<) =  \ f g x y -> do x' <- g x; f x' y

