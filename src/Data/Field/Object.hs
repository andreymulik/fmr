{-# LANGUAGE ConstraintKinds, DataKinds, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE Safe, GADTs, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, TypeFamilies #-}

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

import Data.Kind

import GHC.TypeLits

default ()

infixr 6 :++

--------------------------------------------------------------------------------

data FObject c as
  where
    FObjectEmpty :: FObject c '[]
    (:++)        :: a ~</= as => c a -> FObject c as -> FObject c (a : as)

{-# COMPLETE FObjectElem #-}

pattern FObjectElem :: a ~:= as => c a -> FObject c as
pattern FObjectElem k <- (getFObject -> k)

--------------------------------------------------------------------------------

class IsField field c a
  where
    fromField :: a ~:= as => field as -> c a

instance IsField (FObject c) c a where fromField = getFObject

--------------------------------------------------------------------------------

data family FieldC (m :: Type -> Type) record e (c :: Symbol) :: Type

--------------------------------------------------------------------------------

newtype instance FieldC m record e FieldGetA = FieldGetA
  {fieldGetA :: GetterFor m record e}

type FieldGetA = "fmr.field.get"

type GetterFor m record e = record -> m e

--------------------------------------------------------------------------------

newtype instance FieldC m record e FieldSetA = FieldSetA
  {fieldSetA :: SetterFor m record e}

type FieldSetA = "fmr.field.set"

type SetterFor m record e = record -> e -> m ()

--------------------------------------------------------------------------------

newtype instance FieldC m record e FieldModifyA = FieldModifyA
  {fieldModifyA :: ModifierFor m record e}

type FieldModifyA = "fmr.field.modify"

type ModifierFor m record e = record -> (e -> e) -> m e

--------------------------------------------------------------------------------

newtype instance FieldC m record e FieldModifyMA = FieldModifyMA
  {fieldModifyMA :: ModifierMFor m record e}

type FieldModifyMA = "fmr.field.moifyM"

type ModifierMFor m record e = record -> (e -> m e) -> m e

--------------------------------------------------------------------------------

type family FObjectElem (c :: Symbol) cs
  where
    FObjectElem _   '[ ]   = 'False
    FObjectElem c (c :  _) = 'True
    FObjectElem c (_ : cs) = FObjectElem c cs

type c  ~<= cs = FObjectElem c cs ~ 'True
type c ~</= cs = FObjectElem c cs ~ 'False

--------------------------------------------------------------------------------

class a ~<= as => a ~:= as
  where
    getFObject :: FObject c as -> c a

instance {-# INCOHERENT #-} a ~:= (a : as) where getFObject (a :++ _) = a

instance {-# INCOHERENT #-} (a ~<= (a' : as), a ~:= as) => a ~:= (a' : as)
  where
    getFObject (_ :++ as) = getFObject as


