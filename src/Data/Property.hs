{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE Safe, CPP, GADTs, TypeOperators, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, DataKinds, RankNTypes, KindSignatures #-}

-- For [ghc-8.0 .. ghc-8.4) support.
#if __GLASGOW_HASKELL__ < 840
{-# LANGUAGE TypeInType #-}
#endif

{- |
    License     :  BSD-style
    Module      :  Data.Property
    Copyright   :  (c) Andrey Mulik 2020-2022
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.Property@ new-style properties.
    
    Note that @fmr@ is a very small library and it changes a lot from time to
    time and there is no reasonable way to ensure full, and sometimes even
    partial, backward compatibility between major releases.
-}
module Data.Property
(
  -- * Export
  module Data.Field.Object,
  
  -- * Generalized properties
  Prop ( .., Prop ), IsProp (..), PropertyKind,
  
  -- * Field convention
  IsField (..), FieldC (..),
  
  -- * Basic accessors
  FieldGet,  getRecord, get, FieldSet,    setRecord, set,
  FieldModify, modifyRecord, FieldModifyM, modifyRecordM,
  
  -- * Basic properties
  
  -- ** Set properties
  pattern (:=), pattern (::=), pattern (:=$), pattern (::=$),
  
  -- ** Monadic set properties
  pattern (:<=), pattern (:=<), pattern (:<=$), pattern (:=<$),
  
  -- ** Modify properties
  pattern (:~), pattern (::~), pattern (:~$), pattern (::~$),
  
  pattern (:<~), pattern (:~<), pattern (:<~$), pattern (:~<$),
  
  -- * Field operations
  (+=), (-=), (*=), (=/), (<>=), fieldDiv, fieldMod, fieldQuot, fieldRem
)
where

import Data.Field.Object
import Data.Typeable
import Data.Kind

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

import Control.Monad

default ()

infixl 7 *=, =/, `fieldDiv`, `fieldMod`, `fieldQuot`, `fieldRem`
infixl 6 +=, -=
infixr 6 <>=

--------------------------------------------------------------------------------

{- Generalized property. -}

{- |
  @since 0.2
  
  'Prop' is new, generalized and extensible property (existential) type, which
  may contain any 'IsProp' value.
  
  Use 'Prop' pattern to simplify writing patterns and improve support for older
  versions of the GHC.
-}
data Prop m record
  where
    Property :: (Monad m, IsProp prop) => prop m record -> Prop m record
  deriving ( Typeable )

-- | @since 0.3
pattern Prop :: (Monad m, Typeable m, Typeable record, IsProp prop)
             => prop m record -> Prop m record
pattern Prop prop <- (cast -> Just prop) where Prop = Property

--------------------------------------------------------------------------------

{- |
  @since 0.2
  
  'IsProp' is a property class that allows you to extend @fmr@ syntax.
  
  Since @fmr-0.3@ you can create new property types and use it with existing in
  'set' list of actions.
-}
class Typeable prop => IsProp (prop :: PropertyKind)
  where
    -- | @performProp record prop @ performs an action on @record@ using @prop@.
    performProp :: Monad m => record -> prop m record -> m ()

{- |
  @since 0.2
  
  Service kind synonym.
  
  NOTE: the 'PropertyKind' has changed in @fmr-0.3@.
-}
type PropertyKind = (Type -> Type) -> Type -> Type

{- |
  'set' is the main function in @fmr@, which allows you to describe changing the
  value of a record as a sequence of operations on its fields, e.g.
-}
set :: Monad m => record -> [Prop m record] -> m ()
set record = mapM_ $ \ (Property prop) -> performProp record prop

--------------------------------------------------------------------------------

{- Setter property. -}

{- |
  @since 0.2
  
  'SetProp is a service type used to set field values. See @(':=')@, @(':=$')@,
  @('::=')@, @('::=$')@, @(':<=')@, @(':<=$')@, @(':=<')@ and @(':=<$')@ patterns.
-}
data SetProp m record
  where
    -- | 'SetProp' corresponds to @(':=$')@ and @(':=')@.
    SetProp :: FieldSet field m record e
            => ![field] -> !e -> SetProp m record
    
    -- | 'SetPropM' corresponds to @('::=$')@ and @('::=')@.
    SetPropM :: FieldSet field m record e
             => [field] -> !(m e) -> SetProp m record
    
    -- | 'SetRecordProp' corresponds to @(':<=$')@ and @(':<=')@.
    SetRecordProp :: FieldSet field m record e
                  => ![field] -> !(record -> e) -> SetProp m record
    
    -- | 'SetRecordPropM' corresponds to @(':=<$')@ and @(':=<')@.
    SetRecordPropM :: FieldSet field m record e
                   => ![field] -> !(record -> m e) -> SetProp m record
  deriving ( Typeable )

instance IsProp SetProp
  where
    performProp record (SetRecordPropM field   kl) = setRecords field record =<< kl record
    performProp record (SetRecordProp  field    f) = setRecords field record $ f record
    performProp record (SetPropM       field mval) = setRecords field record =<< mval
    performProp record (SetProp        field  val) = setRecords field record val

-- | Multi-field 'setRecord'.
setRecords :: (Monad m, FieldSet field m record e) => [field] -> record -> e -> m ()
setRecords fields record val = forM_ fields $ \ field -> setRecord field record val

--------------------------------------------------------------------------------

{- |
  @since 0.2
  
  'ModifyProp' is a service type used to modify field values. See @(':~')@,
  @(':~$')@, @(':<~')@, @(':<~$')@, @('::~')@, @('::~$')@, @(':~<')@, @(':~<$')@
  patterns.
-}
data ModifyProp m record
  where
    -- | 'FieldModify' constructor corresponds to @(':~$')@ and @(':~')@.
    ModifyProp :: FieldModify field m record e
               => ![field] -> !(e -> e) -> ModifyProp m record
    
    -- | 'FieldModify' constructor corresponds to @(':<~$')@ and @(':<~')@.
    ModifyPropM :: FieldModifyM field m record e
                => ![field] -> !(e -> m e) -> ModifyProp m record
    
    -- | 'Modify' constructor corresponds to @('::~$')@ and @('::~')@.
    Modify :: FieldModify field m record e
           => ![field] -> !(record -> e -> e) -> ModifyProp m record
    
    -- | 'ModifyM' constructor corresponds to @(':~<$')@ and @(':~<')@.
    ModifyM :: FieldModifyM field m record e
            => ![field] -> !(record -> e -> m e) -> ModifyProp m record
  deriving ( Typeable )

instance IsProp ModifyProp
  where
    performProp record (Modify      field f) = () <$ modifyRecords  field record (f record)
    performProp record (ModifyM     field f) = () <$ modifyRecordsM field record (f record)
    performProp record (ModifyProp  field f) = () <$ modifyRecords  field record f
    performProp record (ModifyPropM field f) = () <$ modifyRecordsM field record f

-- | Multi-value 'modifyRecord'.
modifyRecords :: (Monad m, FieldModify field m record e)
              => [field] -> record -> (e -> e) -> m ()
modifyRecords fields record f = fields `forM_` \ field -> modifyRecord field record f

-- | Multi-value 'modifyRecordM'.
modifyRecordsM :: (Monad m, FieldModifyM field m record e)
               => [field] -> record -> (e -> m e) -> m ()
modifyRecordsM fields record f = fields `forM_` \ field -> modifyRecordM field record f

--------------------------------------------------------------------------------

{- fmr access conventions. -}

{- |
  @since 0.3
  
  Constraint for field with 'FieldGetA' accessor.
-}
type FieldGet field m record e = IsField field (FieldC m record e) FieldGetA

{- |
  @since 0.2
  
  Extract getter from field. Out of class since @fmr-0.3@.
-}
getRecord :: FieldGet field m record e => field -> GetterFor m record e
getRecord =  fieldGetA.fromField

-- | The 'get' function reads current value of a field. Same as 'getRecord'.
get :: FieldGet field m record e => field -> GetterFor m record e
get =  getRecord

{- |
  @since 0.3
  
  Constraint for field with 'FieldSetA' accessor.
-}
type FieldSet field m record e = IsField field (FieldC m record e) FieldSetA

{- |
  @since 0.3
  
  Extract setter from field. Out of class since @fmr-0.3@.
-}
setRecord :: FieldSet field m record e => field -> SetterFor m record e
setRecord =  fieldSetA.fromField

{- |
  @since 0.3
  
  Constraint for field with 'FieldModifyA' accessor.
-}
type FieldModify field m record e = IsField field (FieldC m record e) FieldModifyA

{- |
  @since 0.3
  
  Extract modifier from field. Out of class since @fmr-0.3@.
-}
modifyRecord :: FieldModify field m record e => field -> ModifierFor m record e
modifyRecord =  fieldModifyA.fromField

{- |
  @since 0.3
  
  Constraint for field with 'FieldModifyMA' accessor.
-}
type FieldModifyM field m record e = IsField field (FieldC m record e) FieldModifyMA

{- |
  @since 0.3
  
  Extract monadic modifier from field. Out of class since @fmr-0.3@.
-}
modifyRecordM :: FieldModifyM field m record e => field -> ModifierMFor m record e
modifyRecordM =  fieldModifyMA.fromField

--------------------------------------------------------------------------------

{- |
  Pure value setter. @set record [field := value]@ set @value@ to @record@'s
  @field@.
-}
pattern (:=) :: (Monad m, Typeable m, Typeable record)
             => FieldSet field m record e
             => field -> e -> Prop m record
pattern field := val = Prop (SetProp [field] val)

{- |
  Pure value setter with @record@. @set record [field ::= f]@ set @f record@ to
  @record@'s @field@.
  
  @
    set record [field ::= const val] === set record [field := val]@
-}
pattern (::=) :: (Monad m, Typeable m, Typeable record)
              => FieldSet field m record e
              => field -> (record -> e) -> Prop m record
pattern field ::= f = Prop (SetRecordProp [field] f)

{- |
  @since 0.2
  Pure group setter. @set record [fields :=$ value]@ set @value@ to @record@'s
  some @fields@.
  
  @
    set record [[field] :=$ value] === set record [field := value]@
-}
pattern (:=$) :: (Monad m, Typeable m, Typeable record)
              => FieldSet field m record e
              => [field] -> e -> Prop m record
pattern fields :=$ val = Prop (SetProp fields val)

{- |
  @since 0.2
  Pure group setter with @record@. @set record [fields ::=$ f]@ set @f record@
  to @record@'s some @fields@.
  
  @
    set record [[field] ::=$ f] === set record [field ::= f]
    set record [fields ::=$ const val] === set record [fields :=$ val]@
-}
pattern (::=$) :: (Monad m, Typeable m, Typeable record)
               => FieldSet field m record e
               => [field] -> (record -> e) -> Prop m record
pattern fields ::=$ f = Prop (SetRecordProp fields f)

--------------------------------------------------------------------------------

{- |
  Pure value modifier. @set record [field :~ f]@ modify value of @record@'s
  @field@ using @f@ function.
  
  @
    set record [field :~ const val] === set record [field := val]@
-}
pattern (:~) :: (Monad m, Typeable m, Typeable record)
             => FieldModify field m record e
             => field -> (e -> e) -> Prop m record
pattern field :~ f = Prop (ModifyProp [field] f)

{- |
  Pure value modifier with @record@. @set record [field ::~ f]@ modify value of
  @record@'s @field@ using @f record@ function.
  
  @
    set record [field ::~ const f] === set record [field :~ f]@
-}
pattern (::~) :: (Monad m, Typeable m, Typeable record)
              => FieldModify field m record e
              => field -> (record -> e -> e) -> Prop m record
pattern field ::~ f = Prop (Modify [field] f)

{- |
  @since 0.2
  Pure group modifier. @set record [fields :~$ f]@ modify values of @record@'s
  @fields@ using @f@ function.
  
  @
    set record [[field] :~$ val] === set record [field :~ val]
    set record [fields :~$ const val] === set record [field :=$ val]@
-}
pattern (:~$) :: (Monad m, Typeable m, Typeable record)
              => FieldModify field m record e
              => [field] -> (e -> e) -> Prop m record
pattern fields :~$ f = Prop (ModifyProp fields f)

{- |
  @since 0.2
  Pure group modifier with @record@.
  @set record [fields ::~$ f]@ modify values of @record@'s @fields@ using
  @f record@ function.
  
  @
    set record [[field] ::~$ f] === set record [field ::~ f]
    set record [fields ::~$ const f] === set record [field :~$ f]@
-}
pattern (::~$) :: (Monad m, Typeable m, Typeable record)
               => FieldModify field m record e
               => [field] -> (record -> e -> e) -> Prop m record
pattern fields ::~$ f = Prop (Modify fields f)

--------------------------------------------------------------------------------

{- |
  @since 0.2
  
  Monadic value setter. @set record [field :<= mvalue]@ set result of @mvalue@
  to @record@'s @field@. Note that the @mvalue@ is evaluated every time a
  @field@ value is assigned.
  
  @
    set record [field :<= return val] === set record [field := val]
    set record [field :<= mval] === do val <- mval; set record [field :<= val]@
-}
pattern (:<=) :: (Monad m, Typeable m, Typeable record)
              => FieldSet field m record e
              => field -> m e -> Prop m record
pattern field :<= mval = Prop (SetPropM [field] mval)

{- |
  @since 0.2
  
  Monadic value setter with @record@. @set record [field :=< mvalue]@ set result
  of @mvalue record@ to @record@'s @field@. Note that the @mvalue@ is evaluated
  every time a @field@ value is assigned.
  
  @
    set record [field :=< const val] === set record [field :<= val]
    set record [field :=< f] === do val <- f record; set record [field := val]@
-}
pattern (:=<) :: (Monad m, Typeable m, Typeable record)
              => FieldSet field m record e
              => field -> (record -> m e) -> Prop m record
pattern field :=< f = Prop (SetRecordPropM [field] f)

{- |
  @since 0.2
  
  Monadic group setter. @set record [fields :<=$ mvalue]@ set result of @mvalue@
  to @record@'s @fields@. Note that @mvalue@ is evaluated only once, on the
  first assignment. Thus, the values of all the listed fields will be identical.
  
  @
    set record [[field] :<=$ const f] === set record [field :<= val]
    set record [fields :<=$ mval] === do val <- mval; set record [fields :<=$ val]@
-}
pattern (:<=$) :: (Monad m, Typeable m, Typeable record)
               => FieldSet field m record e
               => [field] -> m e -> Prop m record
pattern fields :<=$ mval = Prop (SetPropM fields mval)

{- |
  @since 0.2
  
  Monadic group setter with @record@. @set record [fields :=<$ f]@ set result of
  @f record@ to @record@'s @fields@. Note that @f record@ is evaluated only
  once, on the first assignment. Thus, the values of all the listed fields will
  be identical.
  
  @
    set record [[field] :=<$ f] === set record [field :=< val]
    set record [fields :=<$ const val] = set record [fields :=$ val]
    set record [fields :=<$ f] === do val <- f record; set record [fields :=$ val]@
-}
pattern (:=<$) :: (Monad m, Typeable m, Typeable record)
               => FieldSet field m record e
               => [field] -> (record -> m e) -> Prop m record
pattern fields :=<$ f = Prop (SetRecordPropM fields f)

--------------------------------------------------------------------------------

{- |
  @since 0.2
  
  Monadic value modifier. @set record [field :<~ f]@ modifies value of
  @record@'s @field@ using @f@ procedure. Note that the @mvalue@ is called every
  time a @field@ value is assigned.
  
  @
    set record [field :<~ return val] === set record [fields := val]@
-}
pattern (:<~) :: (Monad m, Typeable m, Typeable record)
              => FieldModifyM field m record e
              => field -> (e -> m e) -> Prop m record
pattern field :<~ f = Prop (ModifyPropM [field] f)

{- |
  @since 0.2
  
  Monadic value modifier with @record@. @set record [field :<~ f]@ modifies
  value of @record@'s @field@ using @f record@ procedure. Note that the
  @f record@ is called every time e @field@ value is assigned.
  
  @
    set record [field :~< const f] === set record [fields :<~ f]@
-}
pattern (:~<) :: (Monad m, Typeable m, Typeable record)
              => FieldModifyM field m record e
              => field -> (record -> e -> m e) -> Prop m record
pattern field :~< f = Prop (ModifyM [field] f)

{- |
  @since 0.2
  
  Monadic group modifier. @set record [fields :<~$ f]@ modifies values of
  @record@'s @fields@ using @f@ procedure.
  
  @
    set record [[field] :<~$ f] === set record [field :<~ f]
    set record [fields :<~$ const mval] === set record [field :<=$ mval]@
-}
pattern (:<~$) :: (Monad m, Typeable m, Typeable record)
               => FieldModifyM field m record e
               => [field] -> (e -> m e) -> Prop m record
pattern fields :<~$ f = Prop (ModifyPropM fields f)

{- |
  @since 0.2
  
  Monadic group modifier with @record@. @set record [fields :~<$ f]@ modifies
  values of @record@'s @fields@ using @f record@ procedure. Note that the
  @f record@ is called every time a @field@ value is assigned.
  
  @
    set record [field :~<$ const f] === set record [fields :<~$ f]@
-}
pattern (:~<$) :: (Monad m, Typeable m, Typeable record)
               => FieldModifyM field m record e
               => [field] -> (record -> e -> m e) -> Prop m record
pattern fields :~<$ f = Prop (ModifyM fields f)

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Add the given number to the current field value.
-}
(+=) :: (Monad m, FieldModify field m record e, Num e)
     => field -> e -> Prop m record
x += y = Property (ModifyProp [x] (+ y))

{- |
  @since 0.3
  
  Subtract the given number from the current field value.
-}
(-=) :: (Monad m, FieldModify field m record e, Num e)
     => field -> e -> Prop m record
x -= y = Property (ModifyProp [x] (subtract y))

{- |
  Multiply the current field value by the given number.
-}
(*=) :: (Monad m, FieldModify field m record e, Num e)
     => field -> e -> Prop m record
x *= y = Property (ModifyProp [x] (* y))

{- |
  @since 0.3
  
  Apply @('<>')@ to the current value of the field and the passed argument.
-}
(<>=) :: (Monad m, FieldModify field m record e, Semigroup e)
      => field -> e -> Prop m record
x <>= y = Property (ModifyProp [x] (<> y))

{- |
  @since 0.3
  
  Divide the current field value by the passed number.
-}
(=/) :: (Monad m, FieldModify field m record e, Fractional e)
     => field -> e -> Prop m record
x =/ y = Property (ModifyProp [x] (/ y))

{- |
  @since 0.3
  
  Apply 'div' to the current value of the field and the passed argument.
-}
fieldDiv :: (Monad m, FieldModify field m record e, Integral e)
         => field -> e -> Prop m record
fieldDiv x y = Property (ModifyProp [x] (`div` y))

{- |
  @since 0.3
  
  Apply 'mod' to the current value of the field and the passed argument.
-}
fieldMod :: (Monad m, FieldModify field m record e, Integral e)
         => field -> e -> Prop m record
fieldMod x y = Property (ModifyProp [x] (`mod` y))

{- |
  @since 0.3
  
  Apply 'quot' to the current value of the field and the passed argument.
-}
fieldQuot :: (Monad m, FieldModify field m record e, Integral e)
          => field -> e -> Prop m record
fieldQuot x y = Property (ModifyProp [x] (`quot` y))

{- |
  @since 0.3
  
  Apply 'rem' to the current value of the field and the passed argument.
-}
fieldRem :: (Monad m, FieldModify field m record e, Integral e)
         => field -> e -> Prop m record
fieldRem x y = Property (ModifyProp [x] (`rem` y))



