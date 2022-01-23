{-# LANGUAGE CPP, GADTs, PatternSynonyms, ViewPatterns, FlexibleContexts #-}
{-# LANGUAGE Safe, ConstraintKinds, DataKinds, RankNTypes #-}

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
  Prop (..), IsProp (..), PropertyKind,
  
  -- * Field convention
  IsField (..), FieldC (..),
  
  -- ** Basic properties
  FieldGet, FieldGet', FieldGetA, GetterFor, getRecord, get,
  FieldSet, FieldSet', FieldSetA, SetterFor, setRecord, set,
  
  -- *** Set properties
  pattern (:=), pattern (::=), pattern (:=$), pattern (::=$),
  
  -- *** Monadic set properties
  pattern (:<=), pattern (:<=$), pattern (:=<), pattern (:=<$),
  
  -- ** Modify properties
  FieldModify, FieldModify', FieldModifyA, ModifierFor, modifyRecord,
  
  -- *** Modify properties
  pattern (:~), pattern (:~$), pattern (::~), pattern (::~$),
  
  -- ** Monadic modify properties
  FieldModifyM, FieldModifyM', FieldModifyMA, ModifierMFor, modifyRecordM,
  
  -- *** Monadic modify properties
  pattern (:<~), pattern (:<~$), pattern (:~<), pattern (:~<$)
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
data Prop m field record cs
  where
    Property :: (Monad m, IsProp prop)
             => prop m field record cs
             -> Prop m field record cs
  deriving ( Typeable )

--------------------------------------------------------------------------------

{- |
  @since 0.2
  'IsProp is a property class that allows you to extend @fmr@ syntax. Now you
  can create new property types and use it with existing in 'set' list of
  actions.
-}
class IsProp (prop :: PropertyKind k)
  where
    -- | @performProp record prop @ performs an action on @record@ using @prop@.
    performProp :: (Monad m) => record -> prop m field record cs -> m ()

-- | @since 0.2 Service kind synonym.
type PropertyKind k = (Type -> Type) -> ([k] -> Type) -> Type -> [k] -> Type

{- |
  'set' is the main function in @fmr@, which allows you to describe changing the
  value of a record as a sequence of operations on its fields, e.g.
-}
set :: Monad m => record -> [Prop m field record cs] -> m ()
set record = mapM_ $ \ (Property prop) -> performProp record prop

--------------------------------------------------------------------------------

{- Setter property. -}

{- |
  @since 0.2
  'SetProp is a service type used to set field values.
  See @(':=')@, @(':=$')@, @('::=')@, @('::=$')@, @(':<=')@, @(':<=$')@,
  @(':=<')@ and @(':=<$')@ patterns.
-}
data SetProp m field record cs
  where
    -- | 'SetProp corresponds to @(':=$')@ and @(':=')@.
    SetProp :: FieldSet field m record a cs =>
      ![field cs] -> !a -> SetProp m field record cs
    
    -- | 'SetPropM' corresponds to @('::=$')@ and @('::=')@.
    SetPropM :: FieldSet field m record a cs =>
      [field cs] -> !(m a) -> SetProp m field record cs
    
    -- | 'SetRecordProp corresponds to @(':<=$')@ and @(':<=')@.
    SetRecordProp :: FieldSet field m record a cs =>
      ![field cs] -> !(record -> a) -> SetProp m field record cs
    
    -- | 'SetRecordPropM' corresponds to @(':=<$')@ and @(':=<')@.
    SetRecordPropM :: FieldSet field m record a cs =>
      ![field cs] -> !(record -> m a) -> SetProp m field record cs
  deriving ( Typeable )

instance IsProp SetProp
  where
    performProp record (SetRecordPropM field   kl) = setRecords field record =<< kl record
    performProp record (SetRecordProp  field    f) = setRecords field record (f record)
    performProp record (SetPropM       field mval) = setRecords field record =<< mval
    performProp record (SetProp        field  val) = setRecords field record val

setRecords :: (Monad m, FieldSet field m record a as) => [field as] -> record -> a -> m ()
setRecords fields record val = forM_ fields $ \ field -> setRecord field record val

--------------------------------------------------------------------------------

{- |
  @since 0.2
  'ModifyProp is a service type used to modify field values.
  See @(':~')@, @(':~$')@, @(':<~')@, @(':<~$')@, @('::~')@, @('::~$')@,
  @(':~<')@, @(':~<$')@ patterns.
-}
data ModifyProp m field record cs
  where
    -- | 'FieldModify' constructor corresponds to @(':~$')@ and @(':~')@.
    ModifyProp :: FieldModify field m record a cs =>
      ![field cs] -> !(a -> a) -> ModifyProp m field record cs
    
    -- | 'FieldModify' constructor corresponds to @(':<~$')@ and @(':<~')@.
    ModifyPropM :: FieldModifyM field m record a cs =>
      ![field cs] -> !(a -> m a) -> ModifyProp m field record cs
    
    -- | 'Modify' constructor corresponds to @('::~$')@ and @('::~')@.
    Modify :: FieldModify field m record a cs =>
      ![field cs] -> !(record -> a -> a) -> ModifyProp m field record cs
    
    -- | 'ModifyM' constructor corresponds to @(':~<$')@ and @(':~<')@.
    ModifyM :: FieldModifyM field m record a cs =>
      ![field cs] -> !(record -> a -> m a) -> ModifyProp m field record cs
  deriving ( Typeable )

instance IsProp ModifyProp
  where
    performProp record (Modify      field f) = () <$ modifyRecords  field record (f record)
    performProp record (ModifyM     field f) = () <$ modifyRecordsM field record (f record)
    performProp record (ModifyProp  field f) = () <$ modifyRecords  field record f
    performProp record (ModifyPropM field f) = () <$ modifyRecordsM field record f

modifyRecords :: (Monad m, FieldModify field m record a cs) => [field cs] -> record -> (a -> a) -> m ()
modifyRecords fields record f = fields `forM_` \ field -> modifyRecord field record f

modifyRecordsM :: (Monad m, FieldModifyM field m record a cs) =>
  [field cs] -> record -> (a -> m a) -> m ()
modifyRecordsM fields record f = fields `forM_` \ field -> modifyRecordM field record f

--------------------------------------------------------------------------------

{- fmr access conventions. -}

type FieldGet field m record a = IsField field (FieldC m record a) FieldGetA

type FieldGet' field m record = forall a. FieldGet field m record a

getRecord :: FieldGet field m record a cs => field cs -> GetterFor m record a
getRecord =  fieldGetA.fromField

-- | The 'get' function reads current value of a field.
get :: FieldGet field m record a cs => field cs -> GetterFor m record a
get =  getRecord

type FieldSet field m record a = IsField field (FieldC m record a) FieldSetA

type FieldSet' field m record = forall a. FieldSet field m record a

setRecord :: FieldSet field m record a cs => field cs -> SetterFor m record a
setRecord =  fieldSetA.fromField

type FieldModify field m record a = IsField field (FieldC m record a) FieldModifyA

type FieldModify' field m record = forall a. FieldModify field m record a

modifyRecord :: FieldModify field m record a cs => field cs -> ModifierFor m record a
modifyRecord =  fieldModifyA.fromField

type FieldModifyM field m record a = IsField field (FieldC m record a) FieldModifyMA

type FieldModifyM' field m record = forall a. FieldModifyM field m record a

modifyRecordM :: FieldModifyM field m record a cs => field cs -> ModifierMFor m record a
modifyRecordM =  fieldModifyMA.fromField

--------------------------------------------------------------------------------

pattern Prop ::
  (
    Monad m, IsProp prop, Typeable prop, Typeable m, Typeable (cs :: [Symbol]),
    Typeable record, Typeable field
  ) => prop m field record cs -> Prop m field record cs
pattern Prop prop <- (cast -> Just prop) where Prop = Property

{- fmr pure setters. -}

{- |
  Pure value setter. @set record [field := value]@ set @value@ to @record@'s
  @field@.
-}
pattern (:=) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
             => forall a. FieldSet field m record a (cs :: [Symbol])
             => field cs -> a -> Prop m field record cs
pattern field := val = [field] :=$ val

{- |
  Pure value setter with @record@. @set record [field ::= f]@ set @f record@ to
  @record@'s @field@.
  
  @
    set record [field ::= const val] === set record [field := val]
  @
-}
pattern (::=) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
              => forall a. FieldSet field m record a (cs :: [Symbol])
              => field cs -> (record -> a) -> Prop m field record cs
pattern field ::= f = [field] ::=$ f

{- |
  @since 0.2
  Pure group setter. @set record [fields :=$ value]@ set @value@ to @record@'s
  some @fields@.
  
  @
    set record [[field] :=$ value] === set record [field := value]
  @
-}
pattern (:=$) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
              => forall a. FieldSet field m record a (cs :: [Symbol])
              => [field cs] -> a -> Prop m field record cs
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
pattern (::=$) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
               => forall a. FieldSet field m record a (cs :: [Symbol])
               => [field cs] -> (record -> a) -> Prop m field record cs
pattern fields ::=$ f =  Prop (SetRecordProp fields f)

--------------------------------------------------------------------------------

{- fmr pure updaters. -}

{- |
  Pure value modifier. @set record [field :~ f]@ modify value of @record@'s
  @field@ using @f@ function.
  
  @
    set record [field :~ const val] === set record [field := val]
  @
-}
pattern (:~) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
             => forall a. FieldModify field m record a cs
             => field cs -> (a -> a) -> Prop m field record cs
pattern field :~ f = [field] :~$ f

{- |
  Pure value modifier with @record@. @set record [field ::~ f]@ modify value of
  @record@'s @field@ using @f record@ function.
  
  @
    set record [field ::~ const f] === set record [field :~ f]
  @
-}
pattern (::~) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
              => forall a. FieldModify field m record a cs
              => field cs -> (record -> a -> a) -> Prop m field record cs
pattern field ::~ f = [field] ::~$ f

{- |
  @since 0.2
  Pure group modifier. @set record [fields :~$ f]@ modify values of @record@'s
  @fields@ using @f@ function.
  
  @
    set record [[field] :~$ val] === set record [field :~ val]
    set record [fields :~$ const val] === set record [field :=$ val]
  @
-}
pattern (:~$) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
              => forall a. FieldModify field m record a cs
              => [field cs] -> (a -> a) -> Prop m field record cs
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
pattern (::~$) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
               => forall a. FieldModify field m record a cs
               => [field cs] -> (record -> a -> a) -> Prop m field record cs
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
pattern (:<=) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
              => forall a. FieldSet field m record a (cs :: [Symbol])
              => field cs -> m a -> Prop m field record cs
pattern field :<= mval = [field] :<=$ mval

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
pattern (:=<) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
              => forall a. FieldSet field m record a (cs :: [Symbol])
              => field cs -> (record -> m a) -> Prop m field record cs
pattern field :=< f = [field] :=<$ f

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
pattern (:<=$) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
               => forall a. FieldSet field m record a (cs :: [Symbol])
               => [field cs] -> m a -> Prop m field record cs
pattern field :<=$ mval = Prop (SetPropM field mval)

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
pattern (:=<$) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
               => forall a. FieldSet field m record a (cs :: [Symbol])
               => [field cs] -> (record -> m a) -> Prop m field record cs
pattern field :=<$ f = Prop (SetRecordPropM field f)

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
pattern (:<~) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
              => forall a. FieldModifyM field m record a cs
              => field cs -> (a -> m a) -> Prop m field record cs
pattern field :<~ f = [field] :<~$ f

{- |
  @since 0.2
  Monadic value modifier with @record@. @set record [field :<~ f]@ modifies
  value of @record@'s @field@ using @f record@ procedure. Note that the
  @f record@ is called every time a @field@ value is assigned.
  
  @
    set record [field :~< const f] === set record [fields :<~ f]
  @
-}
pattern (:~<) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
              => forall a. FieldModifyM field m record a cs
              => field cs -> (record -> a -> m a) -> Prop m field record cs
pattern field :~< f = [field] :~<$ f

{- |
  @since 0.2
  Monadic group modifier. @set record [fields :<~$ f]@ modifies values of
  @record@'s @fields@ using @f@ procedure.
  
  @
    set record [[field] :<~$ f] === set record [field :<~ f]
    set record [fields :<~$ const mval] === set record [field :<=$ mval]
  @
-}
pattern (:<~$) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
               => forall a. FieldModifyM field m record a cs
               => [field cs] -> (a -> m a) -> Prop m field record cs
pattern field :<~$ f = Prop (ModifyPropM field f)

{- |
  @since 0.2
  Monadic group modifier with @record@. @set record [fields :~<$ f]@ modifies
  values of @record@'s @fields@ using @f record@ procedure. Note that the
  @f record@ is called every time a @field@ value is assigned.
  
  @
    set record [field :~<$ const f] === set record [fields :<~$ f]
  @
-}
pattern (:~<$) :: (Monad m, Typeable m, Typeable field, Typeable cs, Typeable record)
               => forall a. FieldModifyM field m record a cs
               => [field cs] -> (record -> a -> m a) -> Prop m field record cs
pattern field :~<$ f = Prop (ModifyM field f)

