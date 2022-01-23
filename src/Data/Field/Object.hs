{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, GADTs #-}
{-# LANGUAGE Safe, TypeOperators, ConstraintKinds, DataKinds, PolyKinds #-}

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
  FObject (..), IsField (..), type (~<=), type (~</=), type (~:=),
  
  -- * Field convention
  FieldC (..), FieldGetA, FieldSetA, FieldModifyA, FieldModifyMA,
  GetterFor, SetterFor, ModifierFor, ModifierMFor
)
where

import Data.Kind

default ()

infixr 6 :++

--------------------------------------------------------------------------------

data FObject (conv :: k -> Type) (as :: [k])
  where
    FObjectEmpty :: FObject conv '[]
    (:++)        :: a ~</= as => conv a -> FObject conv as -> FObject conv (a : as)

--------------------------------------------------------------------------------

class a ~<= as => IsField field conv (a :: k) as
  where
    fromField :: field as -> conv a

instance a ~:= as => IsField (FObject f) f a as where fromField = getFObject

--------------------------------------------------------------------------------

data family FieldC (m :: Type -> Type) record a (c :: k) :: Type

--------------------------------------------------------------------------------

newtype instance FieldC m record a FieldGetA = FieldGetA
  {fieldGetA :: GetterFor m record a}

type FieldGetA = "fmr.field.get"

type GetterFor m record a = record -> m a

--------------------------------------------------------------------------------

newtype instance FieldC m record a FieldSetA = FieldSetA
  {fieldSetA :: SetterFor m record a}

type FieldSetA = "fmr.field.set"

type SetterFor m record a = record -> a -> m ()

--------------------------------------------------------------------------------

newtype instance FieldC m record a FieldModifyA = FieldModifyA
  {fieldModifyA :: ModifierFor m record a}

type FieldModifyA = "fmr.field.modify"

type ModifierFor m record a = record -> (a -> a) -> m a

--------------------------------------------------------------------------------

newtype instance FieldC m record a FieldModifyMA = FieldModifyMA
  {fieldModifyMA :: ModifierMFor m record a}

type FieldModifyMA = "fmr.field.moifyM"

type ModifierMFor m record a = record -> (a -> m a) -> m a

--------------------------------------------------------------------------------

type family FObjectElem c cs
  where
    FObjectElem _   '[ ]   = 'False
    FObjectElem c (c :  _) = 'True
    FObjectElem c (_ : cs) = FObjectElem c cs

type a  ~<= as = FObjectElem a as ~ 'True
type a ~</= as = FObjectElem a as ~ 'False

--------------------------------------------------------------------------------

class a ~<= as => FObjectGet a as
  where
    getFObject :: FObject f as -> f a

type a ~:= as = FObjectGet a as

instance {-# INCOHERENT #-} FObjectGet a (a : as) where getFObject (a :++ _) = a

instance {-# INCOHERENT #-} (a ~<= (a' : as), FObjectGet a as) => FObjectGet a (a' : as)
  where
    getFObject (_ :++ as) = getFObject as



