{-# LANGUAGE Trustworthy, GADTs, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds, DataKinds, PatternSynonyms, ViewPatterns #-}

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
  FObject ( .., FObjectElem ), IsField (..), type (~:=), type (~<=), type (~</=),
  
  -- * Field convention
  FieldC (..), FieldGetA, FieldSetA, FieldModifyA, FieldModifyMA,
  GetterFor, SetterFor, ModifierFor, ModifierMFor
)
where

import Data.Proxy
import Data.IORef
import Data.STRef
import Data.Kind

import GHC.TypeLits
import GHC.Conc

import Control.Concurrent.MVar
import Control.Monad.ST

default ()

infixr 6 :++

--------------------------------------------------------------------------------

{- |
  @'FObject' c as@ is generalized field representation, where @c@ is convention
  type operation (usually, type family), @as@ - list of stored (supported)
  accessors.
-}
data FObject c as
  where
    FObjectEmpty :: FObject c '[]
    (:++)        :: a ~</= as => c a -> FObject c as -> FObject c (a : as)

{-# COMPLETE FObjectElem #-}

-- | Extract accessor from field, completely useless in older GHC (too generic).
pattern FObjectElem :: a ~:= as => c a -> FObject c as
pattern FObjectElem x <- (getFObject -> x)

--------------------------------------------------------------------------------

{- |
  The @'IsField' field@ class is designed to extract accessors from @field@. As
  a rule, accessors are wrapped in data or newtype, so for a more comfortable
  work with fields, it is recommended to use helper functions like 'getRecord'.
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

--------------------------------------------------------------------------------

instance IsField (Proxy (STRef s e)) (FieldC (ST s) (STRef s e) e) FieldGetA
  where
    fromField _ = FieldGetA readSTRef

instance IsField (Proxy (STRef s e)) (FieldC (ST s) (STRef s e) e) FieldSetA
  where
    fromField _ = FieldSetA writeSTRef

instance IsField (Proxy (STRef s e)) (FieldC (ST s) (STRef s e) e) FieldModifyA
  where
    fromField _ = FieldModifyA $ \ ref f -> do
      res <- f <$> readSTRef ref
      res <$ writeSTRef ref res

instance IsField (Proxy (STRef s e)) (FieldC (ST s) (STRef s e) e) FieldModifyMA
  where
    fromField _ = FieldModifyMA $ \ ref f -> do
      res <- f =<< readSTRef ref
      res <$ writeSTRef ref res

--------------------------------------------------------------------------------

instance IsField (Proxy (IORef e)) (FieldC IO (IORef e) e) FieldGetA
  where
    fromField _ = FieldGetA readIORef

instance IsField (Proxy (IORef e)) (FieldC IO (IORef e) e) FieldSetA
  where
    fromField _ = FieldSetA writeIORef

instance IsField (Proxy (IORef e)) (FieldC IO (IORef e) e) FieldModifyA
  where
    fromField _ = FieldModifyA $ \ ref f ->
      atomicModifyIORef' ref (\ a -> let b = f a in (b, b))

instance IsField (Proxy (IORef e)) (FieldC IO (IORef e) e) FieldModifyMA
  where
    fromField _ = FieldModifyMA $ \ ref f -> do
      val <- f =<< readIORef ref
      val <$ writeIORef ref val

--------------------------------------------------------------------------------

instance IsField (Proxy (MVar e)) (FieldC IO (MVar e) e) FieldGetA
  where
    fromField _ = FieldGetA readMVar

instance IsField (Proxy (MVar e)) (FieldC IO (MVar e) e) FieldSetA
  where
    fromField _ = FieldSetA putMVar

instance IsField (Proxy (MVar e)) (FieldC IO (MVar e) e) FieldModifyA
  where
    fromField _ = FieldModifyA $ \ mvar f ->
      modifyMVar mvar $ \ a -> let b = f a in return (b, b)

instance IsField (Proxy (MVar e)) (FieldC IO (MVar e) e) FieldModifyMA
  where
    fromField _ = FieldModifyMA $ \ mvar f ->
      modifyMVarMasked mvar $ \ a -> do b <- f a; return (b, b)

--------------------------------------------------------------------------------

instance IsField (Proxy (TVar e)) (FieldC STM (TVar e) e) FieldGetA
  where
    fromField _ = FieldGetA readTVar

instance IsField (Proxy (TVar e)) (FieldC STM (TVar e) e) FieldSetA
  where
    fromField _ = FieldSetA writeTVar

instance IsField (Proxy (TVar e)) (FieldC STM (TVar e) e) FieldModifyA
  where
    fromField _ = FieldModifyA $ \ tvar f -> do
      res <- f <$> readTVar tvar
      res <$ writeTVar tvar res

instance IsField (Proxy (TVar e)) (FieldC STM (TVar e) e) FieldModifyMA
  where
    fromField _ = FieldModifyMA $ \ tvar f -> do
      res <- f =<< readTVar tvar
      res <$ writeTVar tvar res

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



