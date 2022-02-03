{-# LANGUAGE CPP, GADTs, PatternSynonyms, ViewPatterns, FlexibleContexts #-}
{-# LANGUAGE Safe, ConstraintKinds, DataKinds, RankNTypes, TypeOperators #-}

-- For [ghc-8.0 .. ghc-8.4) support.
#if __GLASGOW_HASKELL__ < 840
{-# LANGUAGE TypeInType #-}
#endif

{- |
    License     :  BSD-style
    Module      :  Data.Property
    Copyright   :  (c) Andrey Mulik 2020
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.Property@ new-style properties.
-}
module Data.Property
(
  -- * Generalized properties
  Prop ( .., Prop ), IsProp (..), PropertyKind,
  
  -- * Field convention
  IsField (..), FieldC (..),
  
  -- ** Basic properties
  FieldGet, FieldGetA, GetterFor, getRecord, get,
  FieldSet, FieldSetA, SetterFor, setRecord, set,
  
  -- *** Set properties
  pattern (:=), pattern (::=), pattern (:=$), pattern (::=$),
  
  -- *** Monadic set properties
  pattern (:<=), pattern (:=<), pattern (:<=$), pattern (:=<$),
  
  -- ** Modify properties
  FieldModify, FieldModifyA, ModifierFor, modifyRecord,
  
  -- *** Modify properties
  pattern (:~), pattern (::~), pattern (:~$), pattern (::~$),
  
  -- ** Monadic modify properties
  FieldModifyM, FieldModifyMA, ModifierMFor, modifyRecordM, modifyRecordsM,
  
  -- *** Monadic modify properties
  pattern (:<~), pattern (:~<), pattern (:<~$), pattern (:~<$)
)
where

import Data.Field.Object
import Data.Typeable
import Data.Kind

import GHC.TypeLits

import Control.Monad

default ()

--------------------------------------------------------------------------------

{- Generalized property. -}

{- |
  @since 0.2
  'Prop is new, generalized and extensible property type (existential), which
  may contain any 'IsProp value.
-}
data Prop m field record (as :: [Symbol])
  where
    Property :: (Monad m, IsProp prop)
             => prop m field record as
             -> Prop m field record as
  deriving ( Typeable )

pattern Prop ::
  (
    Monad m, IsProp prop, Typeable prop, Typeable m, Typeable as,
    Typeable record, Typeable field
  ) => prop m field record as -> Prop m field record as
pattern Prop prop <- (cast -> Just prop) where Prop = Property

--------------------------------------------------------------------------------

{- |
  @since 0.2
  'IsProp is a property class that allows you to extend @fmr@ syntax. Now you
  can create new property types and use it with existing in 'set' list of
  actions.
-}
class IsProp (prop :: PropertyKind)
  where
    -- | @performProp record prop @ performs an action on @record@ using @prop@.
    performProp :: Monad m => record -> prop m field record as -> m ()

-- | @since 0.2 Service kind synonym.
type PropertyKind = (Type -> Type) -> ([Symbol] -> Type) -> Type -> [Symbol] -> Type

{- |
  'set' is the main function in @fmr@, which allows you to describe changing the
  value of a record as a sequence of operations on its fields, e.g.
-}
set :: Monad m => record -> [Prop m field record as] -> m ()
set record = mapM_ $ \ (Property prop) -> performProp record prop

--------------------------------------------------------------------------------

{- Setter property. -}

{- |
  @since 0.2
  'SetProp is a service type used to set field values.
  See @(':=')@, @(':=$')@, @('::=')@, @('::=$')@, @(':<=')@, @(':<=$')@,
  @(':=<')@ and @(':=<$')@ patterns.
-}
data SetProp m field record as
  where
    -- | 'SetProp corresponds to @(':=$')@ and @(':=')@.
    SetProp :: FieldSet field m record e as =>
      ![field as] -> !e -> SetProp m field record as
    
    -- | 'SetPropM' corresponds to @('::=$')@ and @('::=')@.
    SetPropM :: FieldSet field m record e as =>
      [field as] -> !(m e) -> SetProp m field record as
    
    -- | 'SetRecordProp corresponds to @(':<=$')@ and @(':<=')@.
    SetRecordProp :: FieldSet field m record e as =>
      ![field as] -> !(record -> e) -> SetProp m field record as
    
    -- | 'SetRecordPropM' corresponds to @(':=<$')@ and @(':=<')@.
    SetRecordPropM :: FieldSet field m record e as =>
      ![field as] -> !(record -> m e) -> SetProp m field record as
  deriving ( Typeable )

instance IsProp SetProp
  where
    performProp record (SetRecordPropM field   kl) = setRecords field record =<< kl record
    performProp record (SetRecordProp  field    f) = setRecords field record $ f record
    performProp record (SetPropM       field mval) = setRecords field record =<< mval
    performProp record (SetProp        field  val) = setRecords field record val

setRecords :: (Monad m, FieldSet field m record e as) => [field as] -> record -> e -> m ()
setRecords fields record val = forM_ fields $ \ field -> setRecord field record val

--------------------------------------------------------------------------------

{- |
  @since 0.2
  'ModifyProp is a service type used to modify field values.
  See @(':~')@, @(':~$')@, @(':<~')@, @(':<~$')@, @('::~')@, @('::~$')@,
  @(':~<')@, @(':~<$')@ patterns.
-}
data ModifyProp m field record as
  where
    -- | 'FieldModify' constructor corresponds to @(':~$')@ and @(':~')@.
    ModifyProp :: FieldModify field m record e as =>
      ![field as] -> !(e -> e) -> ModifyProp m field record as
    
    -- | 'FieldModify' constructor corresponds to @(':<~$')@ and @(':<~')@.
    ModifyPropM :: FieldModifyM field m record e as =>
      ![field as] -> !(e -> m e) -> ModifyProp m field record as
    
    -- | 'Modify' constructor corresponds to @('::~$')@ and @('::~')@.
    Modify :: FieldModify field m record e as =>
      ![field as] -> !(record -> e -> e) -> ModifyProp m field record as
    
    -- | 'ModifyM' constructor corresponds to @(':~<$')@ and @(':~<')@.
    ModifyM :: FieldModifyM field m record e as =>
      ![field as] -> !(record -> e -> m e) -> ModifyProp m field record as
  deriving ( Typeable )

instance IsProp ModifyProp
  where
    performProp record (Modify      field f) = () <$ modifyRecords  field record (f record)
    performProp record (ModifyM     field f) = () <$ modifyRecordsM field record (f record)
    performProp record (ModifyProp  field f) = () <$ modifyRecords  field record f
    performProp record (ModifyPropM field f) = () <$ modifyRecordsM field record f

modifyRecords :: (Monad m, FieldModify field m record e as) => [field as] -> record -> (e -> e) -> m ()
modifyRecords fields record f = fields `forM_` \ field -> modifyRecord field record f

modifyRecordsM :: (Monad m, FieldModifyM field m record e as) =>
  [field as] -> record -> (e -> m e) -> m ()
modifyRecordsM fields record f = fields `forM_` \ field -> modifyRecordM field record f

--------------------------------------------------------------------------------

{- fmr access conventions. -}

type FieldGet field m record e as = (FieldGetA ~:= as, IsField field (FieldC m record e) FieldGetA)

getRecord :: FieldGet field m record e as => field as -> GetterFor m record e
getRecord =  fieldGetA.fromField

-- | The 'get' function reads current value of a field.
get :: FieldGet field m record e as => field as -> GetterFor m record e
get =  getRecord

type FieldSet field m record e as = (FieldSetA ~:= as, IsField field (FieldC m record e) FieldSetA)

setRecord :: FieldSet field m record e as => field as -> SetterFor m record e
setRecord =  fieldSetA.fromField

type FieldModify field m record e as = (FieldModifyA ~:= as, IsField field (FieldC m record e) FieldModifyA)

modifyRecord :: FieldModify field m record e as => field as -> ModifierFor m record e
modifyRecord =  fieldModifyA.fromField

type FieldModifyM field m record e as = (FieldModifyMA ~:= as, IsField field (FieldC m record e) FieldModifyMA)

modifyRecordM :: FieldModifyM field m record e as => field as -> ModifierMFor m record e
modifyRecordM =  fieldModifyMA.fromField

--------------------------------------------------------------------------------

{- fmr pure setters. -}

{- |
  Pure value setter. @set record [field := value]@ set @value@ to @record@'s
  @field@.
-}
pattern (:=) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
             => forall e. FieldSet field m record e as
             => field as -> e -> Prop m field record as
pattern field := val = Prop (SetProp [field] val)

{- |
  Pure value setter with @record@. @set record [field ::= f]@ set @f record@ to
  @record@'s @field@.
  
  @
    set record [field ::= const val] === set record [field := val]
  @
-}
pattern (::=) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
              => forall e. FieldSet field m record e as
              => field as -> (record -> e) -> Prop m field record as
pattern field ::= f = Prop (SetRecordProp [field] f)

{- |
  @since 0.2
  Pure group setter. @set record [fields :=$ value]@ set @value@ to @record@'s
  some @fields@.
  
  @
    set record [[field] :=$ value] === set record [field := value]
  @
-}
pattern (:=$) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
              => forall e. FieldSet field m record e as
              => [field as] -> e -> Prop m field record as
pattern fields :=$ val = Prop (SetProp fields val)

{- |
  @since 0.2
  Pure group setter with @record@. @set record [fields ::=$ f]@ set @f record@
  to @record@'s some @fields@.
  
  @
    set record [[field] ::=$ f] === set record [field ::= f]
    set record [fields ::=$ const val] === set record [fields :=$ val]
  @
-}
pattern (::=$) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
               => forall e. FieldSet field m record e as
               => [field as] -> (record -> e) -> Prop m field record as
pattern fields ::=$ f = Prop (SetRecordProp fields f)

--------------------------------------------------------------------------------

{- fmr pure updaters. -}

{- |
  Pure value modifier. @set record [field :~ f]@ modify value of @record@'s
  @field@ using @f@ function.
  
  @
    set record [field :~ const val] === set record [field := val]
  @
-}
pattern (:~) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
             => forall e. FieldModify field m record e as
             => field as -> (e -> e) -> Prop m field record as
pattern field :~ f = Prop (ModifyProp [field] f)

{- |
  Pure value modifier with @record@. @set record [field ::~ f]@ modify value of
  @record@'s @field@ using @f record@ function.
  
  @
    set record [field ::~ const f] === set record [field :~ f]
  @
-}
pattern (::~) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
              => forall e. FieldModify field m record e as
              => field as -> (record -> e -> e) -> Prop m field record as
pattern field ::~ f = Prop (Modify [field] f)

{- |
  @since 0.2
  Pure group modifier. @set record [fields :~$ f]@ modify values of @record@'s
  @fields@ using @f@ function.
  
  @
    set record [[field] :~$ val] === set record [field :~ val]
    set record [fields :~$ const val] === set record [field :=$ val]
  @
-}
pattern (:~$) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
              => forall e. FieldModify field m record e as
              => [field as] -> (e -> e) -> Prop m field record as
pattern fields :~$ f = Prop (ModifyProp fields f)

{- |
  @since 0.2
  Pure group modifier with @record@.
  @set record [fields ::~$ f]@ modify values of @record@'s @fields@ using
  @f record@ function.
  
  @
    set record [[field] ::~$ f] === set record [field ::~ f]
    set record [fields ::~$ const f] === set record [field :~$ f]
  @
-}
pattern (::~$) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
               => forall e. FieldModify field m record e as
               => [field as] -> (record -> e -> e) -> Prop m field record as
pattern fields ::~$ f = Prop (Modify fields f)

--------------------------------------------------------------------------------

{- fmr monadic setters. -}

{- |
  @since 0.2
  Monadic value setter. @set record [field :<= mvalue]@ set result of @mvalue@
  to @record@'s @field@. Note that the @mvalue@ is evaluated every time a
  @field@ value is assigned.
  
  @
    set record [field :<= return val] === set record [field := val]
    set record [field :<= mval] === do val <- mval; set record [field :<= val]
  @
-}
pattern (:<=) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
              => forall e. FieldSet field m record e as
              => field as -> m e -> Prop m field record as
pattern field :<= mval = Prop (SetPropM [field] mval)

{- |
  @since 0.2
  Monadic value setter with @record@. @set record [field :=< mvalue]@ set result
  of @mvalue record@ to @record@'s @field@. Note that the @mvalue@ is evaluated
  every time a @field@ value is assigned.
  
  @
    set record [field :=< const val] === set record [field :<= val]
    set record [field :=< f] === do val <- f record; set record [field := val]
  @
-}
pattern (:=<) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
              => forall e. FieldSet field m record e as
              => field as -> (record -> m e) -> Prop m field record as
pattern field :=< f = Prop (SetRecordPropM [field] f)

{- |
  @since 0.2
  Monadic group setter. @set record [fields :<=$ mvalue]@ set result of @mvalue@
  to @record@'s @fields@. Note that @mvalue@ is evaluated only once, on the
  first assignment. Thus, the values of all the listed fields will be identical.
  
  @
    set record [[field] :<=$ const f] === set record [field :<= val]
    set record [fields :<=$ mval] === do val <- mval; set record [fields :<=$ val]
  @
-}
pattern (:<=$) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
               => forall e. FieldSet field m record e as
               => [field as] -> m e -> Prop m field record as
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
    set record [fields :=<$ f] === do val <- f record; set record [fields :=$ val]
  @
-}
pattern (:=<$) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
               => forall e. FieldSet field m record e as
               => [field as] -> (record -> m e) -> Prop m field record as
pattern fields :=<$ f = Prop (SetRecordPropM fields f)

--------------------------------------------------------------------------------

{- fmr monadic updaters. -}

{- |
  @since 0.2
  Monadic value modifier. @set record [field :<~ f]@ modifies value of
  @record@'s @field@ using @f@ procedure. Note that the @mvalue@ is called every
  time a @field@ value is assigned.
  
  @
    set record [field :<~ return val] === set record [fields := val]
  @
-}
pattern (:<~) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
              => forall e. FieldModifyM field m record e as
              => field as -> (e -> m e) -> Prop m field record as
pattern field :<~ f = Prop (ModifyPropM [field] f)

{- |
  @since 0.2
  Monadic value modifier with @record@. @set record [field :<~ f]@ modifies
  value of @record@'s @field@ using @f record@ procedure. Note that the
  @f record@ is called every time e @field@ value is assigned.
  
  @
    set record [field :~< const f] === set record [fields :<~ f]
  @
-}
pattern (:~<) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
              => forall e. FieldModifyM field m record e as
              => field as -> (record -> e -> m e) -> Prop m field record as
pattern field :~< f = Prop (ModifyM [field] f)

{- |
  @since 0.2
  Monadic group modifier. @set record [fields :<~$ f]@ modifies values of
  @record@'s @fields@ using @f@ procedure.
  
  @
    set record [[field] :<~$ f] === set record [field :<~ f]
    set record [fields :<~$ const mval] === set record [field :<=$ mval]
  @
-}
pattern (:<~$) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
               => forall e. FieldModifyM field m record e as
               => [field as] -> (e -> m e) -> Prop m field record as
pattern fields :<~$ f = Prop (ModifyPropM fields f)

{- |
  @since 0.2
  Monadic group modifier with @record@. @set record [fields :~<$ f]@ modifies
  values of @record@'s @fields@ using @f record@ procedure. Note that the
  @f record@ is called every time a @field@ value is assigned.
  
  @
    set record [field :~<$ const f] === set record [fields :<~$ f]
  @
-}
pattern (:~<$) :: (Monad m, Typeable m, Typeable field, Typeable as, Typeable record)
               => forall e. FieldModifyM field m record e as
               => [field as] -> (record -> e -> m e) -> Prop m field record as
pattern fields :~<$ f = Prop (ModifyM fields f)




