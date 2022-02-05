{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE CPP, GADTs, PatternSynonyms, ViewPatterns, FlexibleContexts #-}
{-# LANGUAGE Safe, ConstraintKinds, DataKinds, RankNTypes, TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

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
-}
module Data.Property
(
  -- * Generalized properties
  Prop ( .., Prop ), IsProp (..), PropertyKind, CastableProp,
  
  -- * Field convention
  IsField (..), FieldC (..),
  
  -- * Basic accessors
  FieldGet,     FieldGetA,     GetterFor,    getRecord,     get,
  FieldSet,     FieldSetA,     SetterFor,    setRecord,     set,
  FieldModify,  FieldModifyA,  ModifierFor,  modifyRecord,
  FieldModifyM, FieldModifyMA, ModifierMFor, modifyRecordM,
  
  -- * Basic properties
  
  -- ** Set properties
  pattern (:=), pattern (::=), pattern (:=$), pattern (::=$),
  
  -- ** Monadic set properties
  pattern (:<=), pattern (:=<), pattern (:<=$), pattern (:=<$),
  
  -- ** Modify properties
  pattern (:~), pattern (::~), pattern (:~$), pattern (::~$),
  
  -- ** Monadic modify properties
  pattern (:<~), pattern (:~<), pattern (:<~$), pattern (:~<$),
  
  -- * Field operations
  FieldAdd (..), FieldSub (..), FieldNum (..),
  FieldIntegral (..), FieldFractional (..)
)
where

import Data.Field.Object
import Data.Typeable
import Data.Kind

import Control.Monad

default ()

--------------------------------------------------------------------------------

{- Generalized property. -}

{- |
  @since 0.2
  
  'Prop' is new, generalized and extensible property (existential) type, which
  may contain any 'IsProp' value.
  
  Use 'Prop' pattern to simplify writing patterns and improve support for older
  versions of the GHC.
-}
data Prop m field record
  where
    Property :: (Monad m, IsProp prop) => prop m field record -> Prop m field record
  deriving ( Typeable )

-- | @since 0.3
pattern Prop ::
  (
    Typeable prop, Typeable m, Typeable record, Typeable field,
    Monad m, IsProp prop
  ) => prop m field record -> Prop m field record
pattern Prop prop <- (cast -> Just prop) where Prop = Property

--------------------------------------------------------------------------------

{- |
  @since 0.2
  
  'IsProp' is a property class that allows you to extend @fmr@ syntax. Now you
  can create new property types and use it with existing in 'set' list of
  actions.
  
  NOTE: 'IsProp' instance doesn't really have to be so general, if you want to
  implement a less polymorphic property, look at the 'Data.MField.LinkProp'.
-}
class IsProp (prop :: PropertyKind)
  where
    -- | @performProp record prop @ performs an action on @record@ using @prop@.
    performProp :: Monad m => record -> prop m field record -> m ()

{- |
  @since 0.2
  
  Service kind synonym.
  
  NOTE: that the 'PropertyKind' has changed in @fmr-0.3@.
-}
type PropertyKind = (Type -> Type) -> Type -> Type -> Type

{- |
  @since 0.3
  
  Service constraint.
-}
type CastableProp field (m :: Type -> Type) record =
  (Typeable m, Typeable field, Typeable record)

{- |
  'set' is the main function in @fmr@, which allows you to describe changing the
  value of a record as a sequence of operations on its fields, e.g.
-}
set :: Monad m => record -> [Prop m field record] -> m ()
set record = mapM_ $ \ (Property prop) -> performProp record prop

--------------------------------------------------------------------------------

{- Setter property. -}

{- |
  @since 0.2
  'SetProp is a service type used to set field values. See @(':=')@, @(':=$')@,
  @('::=')@, @('::=$')@, @(':<=')@, @(':<=$')@, @(':=<')@ and @(':=<$')@ patterns.
-}
data SetProp m field record
  where
    -- | 'SetProp' corresponds to @(':=$')@ and @(':=')@.
    SetProp :: (FieldSet field m record e)
            => ![field] -> !e -> SetProp m field record
    
    -- | 'SetPropM' corresponds to @('::=$')@ and @('::=')@.
    SetPropM :: (FieldSet field m record e)
             => [field] -> !(m e) -> SetProp m field record
    
    -- | 'SetRecordProp' corresponds to @(':<=$')@ and @(':<=')@.
    SetRecordProp :: (FieldSet field m record e)
                  => ![field] -> !(record -> e) -> SetProp m field record
    
    -- | 'SetRecordPropM' corresponds to @(':=<$')@ and @(':=<')@.
    SetRecordPropM :: (FieldSet field m record e)
                   => ![field] -> !(record -> m e) -> SetProp m field record
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
data ModifyProp m field record
  where
    -- | 'FieldModify' constructor corresponds to @(':~$')@ and @(':~')@.
    ModifyProp :: (FieldModify field m record e)
               => ![field] -> !(e -> e) -> ModifyProp m field record
    
    -- | 'FieldModify' constructor corresponds to @(':<~$')@ and @(':<~')@.
    ModifyPropM :: (FieldModifyM field m record e)
                => ![field] -> !(e -> m e) -> ModifyProp m field record
    
    -- | 'Modify' constructor corresponds to @('::~$')@ and @('::~')@.
    Modify :: (FieldModify field m record e)
           => ![field] -> !(record -> e -> e) -> ModifyProp m field record
    
    -- | 'ModifyM' constructor corresponds to @(':~<$')@ and @(':~<')@.
    ModifyM :: (FieldModifyM field m record e)
            => ![field] -> !(record -> e -> m e) -> ModifyProp m field record
  deriving ( Typeable )

instance IsProp ModifyProp
  where
    performProp record (Modify      field f) = () <$ modifyRecords  field record (f record)
    performProp record (ModifyM     field f) = () <$ modifyRecordsM field record (f record)
    performProp record (ModifyProp  field f) = () <$ modifyRecords  field record f
    performProp record (ModifyPropM field f) = () <$ modifyRecordsM field record f

-- | Multi-value 'modifyRecord'.
modifyRecords :: (Monad m, FieldModify field m record e) => [field] -> record -> (e -> e) -> m ()
modifyRecords fields record f = fields `forM_` \ field -> modifyRecord field record f

-- | Multi-value 'modifyRecordM'.
modifyRecordsM :: (Monad m, FieldModifyM field m record e) =>
  [field] -> record -> (e -> m e) -> m ()
modifyRecordsM fields record f = fields `forM_` \ field -> modifyRecordM field record f

--------------------------------------------------------------------------------

{- fmr access conventions. -}

{- |
  @since 0.3
  
  Constraint for field with 'FieldGetA' accessor.
-}
type FieldGet field m record e = (FieldGetA ~?= field, IsField field (FieldC m record e) FieldGetA)

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
type FieldSet field m record e = (FieldSetA ~?= field, IsField field (FieldC m record e) FieldSetA)

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
type FieldModify field m record e = (FieldModifyA ~?= field, IsField field (FieldC m record e) FieldModifyA)

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
type FieldModifyM field m record e = (FieldModifyMA ~?= field, IsField field (FieldC m record e) FieldModifyMA)

{- |
  @since 0.3
  
  Extract monadic modifier from field. Out of class since @fmr-0.3@.
-}
modifyRecordM :: FieldModifyM field m record e => field -> ModifierMFor m record e
modifyRecordM =  fieldModifyMA.fromField

--------------------------------------------------------------------------------

{- fmr pure setters. -}

{- |
  Pure value setter. @set record [field := value]@ set @value@ to @record@'s
  @field@.
-}
pattern (:=) :: (Monad m, CastableProp field m record)
             => forall e. FieldSet field m record e
             => field -> e -> Prop m field record
pattern field := val = Prop (SetProp [field] val)

{- |
  Pure value setter with @record@. @set record [field ::= f]@ set @f record@ to
  @record@'s @field@.
  
  @
    set record [field ::= const val] === set record [field := val]@
-}
pattern (::=) :: (Monad m, CastableProp field m record)
              => forall e. FieldSet field m record e
              => field -> (record -> e) -> Prop m field record
pattern field ::= f = Prop (SetRecordProp [field] f)

{- |
  @since 0.2
  Pure group setter. @set record [fields :=$ value]@ set @value@ to @record@'s
  some @fields@.
  
  @
    set record [[field] :=$ value] === set record [field := value]@
-}
pattern (:=$) :: (Monad m, CastableProp field m record)
              => forall e. FieldSet field m record e
              => [field] -> e -> Prop m field record
pattern fields :=$ val = Prop (SetProp fields val)

{- |
  @since 0.2
  Pure group setter with @record@. @set record [fields ::=$ f]@ set @f record@
  to @record@'s some @fields@.
  
  @
    set record [[field] ::=$ f] === set record [field ::= f]
    set record [fields ::=$ const val] === set record [fields :=$ val]@
-}
pattern (::=$) :: (Monad m, CastableProp field m record)
               => forall e. FieldSet field m record e
               => [field] -> (record -> e) -> Prop m field record
pattern fields ::=$ f = Prop (SetRecordProp fields f)

--------------------------------------------------------------------------------

{- fmr pure updaters. -}

{- |
  Pure value modifier. @set record [field :~ f]@ modify value of @record@'s
  @field@ using @f@ function.
  
  @
    set record [field :~ const val] === set record [field := val]@
-}
pattern (:~) :: (Monad m, CastableProp field m record)
             => forall e. FieldModify field m record e
             => field -> (e -> e) -> Prop m field record
pattern field :~ f = Prop (ModifyProp [field] f)

{- |
  Pure value modifier with @record@. @set record [field ::~ f]@ modify value of
  @record@'s @field@ using @f record@ function.
  
  @
    set record [field ::~ const f] === set record [field :~ f]@
-}
pattern (::~) :: (Monad m, CastableProp field m record)
              => forall e. FieldModify field m record e
              => field -> (record -> e -> e) -> Prop m field record
pattern field ::~ f = Prop (Modify [field] f)

{- |
  @since 0.2
  Pure group modifier. @set record [fields :~$ f]@ modify values of @record@'s
  @fields@ using @f@ function.
  
  @
    set record [[field] :~$ val] === set record [field :~ val]
    set record [fields :~$ const val] === set record [field :=$ val]@
-}
pattern (:~$) :: (Monad m, CastableProp field m record)
              => forall e. FieldModify field m record e
              => [field] -> (e -> e) -> Prop m field record
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
pattern (::~$) :: (Monad m, CastableProp field m record)
               => forall e. FieldModify field m record e
               => [field] -> (record -> e -> e) -> Prop m field record
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
    set record [field :<= mval] === do val <- mval; set record [field :<= val]@
-}
pattern (:<=) :: (Monad m, CastableProp field m record)
              => forall e. FieldSet field m record e
              => field -> m e -> Prop m field record
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
pattern (:=<) :: (Monad m, CastableProp field m record)
              => forall e. FieldSet field m record e
              => field -> (record -> m e) -> Prop m field record
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
pattern (:<=$) :: (Monad m, CastableProp field m record)
               => forall e. FieldSet field m record e
               => [field] -> m e -> Prop m field record
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
pattern (:=<$) :: (Monad m, CastableProp field m record)
               => forall e. FieldSet field m record e
               => [field] -> (record -> m e) -> Prop m field record
pattern fields :=<$ f = Prop (SetRecordPropM fields f)

--------------------------------------------------------------------------------

{- fmr monadic updaters. -}

{- |
  @since 0.2
  
  Monadic value modifier. @set record [field :<~ f]@ modifies value of
  @record@'s @field@ using @f@ procedure. Note that the @mvalue@ is called every
  time a @field@ value is assigned.
  
  @
    set record [field :<~ return val] === set record [fields := val]@
-}
pattern (:<~) :: (Monad m, CastableProp field m record)
              => forall e. FieldModifyM field m record e
              => field -> (e -> m e) -> Prop m field record
pattern field :<~ f = Prop (ModifyPropM [field] f)

{- |
  @since 0.2
  
  Monadic value modifier with @record@. @set record [field :<~ f]@ modifies
  value of @record@'s @field@ using @f record@ procedure. Note that the
  @f record@ is called every time e @field@ value is assigned.
  
  @
    set record [field :~< const f] === set record [fields :<~ f]@
-}
pattern (:~<) :: (Monad m, CastableProp field m record)
              => forall e. FieldModifyM field m record e
              => field -> (record -> e -> m e) -> Prop m field record
pattern field :~< f = Prop (ModifyM [field] f)

{- |
  @since 0.2
  
  Monadic group modifier. @set record [fields :<~$ f]@ modifies values of
  @record@'s @fields@ using @f@ procedure.
  
  @
    set record [[field] :<~$ f] === set record [field :<~ f]
    set record [fields :<~$ const mval] === set record [field :<=$ mval]@
-}
pattern (:<~$) :: (Monad m, CastableProp field m record)
               => forall e. FieldModifyM field m record e
               => [field] -> (e -> m e) -> Prop m field record
pattern fields :<~$ f = Prop (ModifyPropM fields f)

{- |
  @since 0.2
  
  Monadic group modifier with @record@. @set record [fields :~<$ f]@ modifies
  values of @record@'s @fields@ using @f record@ procedure. Note that the
  @f record@ is called every time a @field@ value is assigned.
  
  @
    set record [field :~<$ const f] === set record [fields :<~$ f]@
-}
pattern (:~<$) :: (Monad m, CastableProp field m record)
               => forall e. FieldModifyM field m record e
               => [field] -> (record -> e -> m e) -> Prop m field record
pattern fields :~<$ f = Prop (ModifyM fields f)

--------------------------------------------------------------------------------

class Monad m => FieldAdd field m record e | field -> m, field -> record, field -> e
  where
    (+=) :: (FieldModifyA ~?= field, FieldGetA ~?= field)
         => field -> field -> record -> m (Prop m field record)

class Monad m => FieldSub field m record e | field -> m, field -> record, field -> e
  where
    (-=) :: (FieldModifyA ~?= field, FieldGetA ~?= field)
         => field -> field -> record -> m (Prop m field record)

class (Num n, FieldAdd field m record n, FieldSub field m record n) => FieldNum field m record n
  where
    (*=) :: (FieldGetA ~?= field, FieldModifyA ~?= field)
         => field -> field -> record -> m (Prop m field record)

--------------------------------------------------------------------------------

instance (Num n, Monad m) => FieldAdd (FObject (FieldC m record n) as) m record n
  where
    x += y = fmap (Property . ModifyProp [x] . (+)) . getRecord y

instance (Num n, Monad m) => FieldSub (FObject (FieldC m record n) as) m record n
  where
    x -= y = fmap (Property . ModifyProp [x] . subtract) . getRecord y

instance (Num n, Monad m) => FieldNum (FObject (FieldC m record n) as) m record n
  where
    x *= y = fmap (Property . ModifyProp [x] . (*)) . getRecord y

--------------------------------------------------------------------------------

class (FieldNum field m record i, Integral i) => FieldIntegral field m record i
  where
    fieldDiv, fieldMod, fieldQuot, fieldRem
      :: (FieldGetA ~?= field, FieldModifyA ~?= field)
      => field -> field -> record -> m (Prop m field record)

instance (Monad m, Integral i) => FieldIntegral (FObject (FieldC m record i) as) m record i
  where
    fieldDiv  x y = fmap (Property . ModifyProp [x] . flip  div) . getRecord y
    fieldMod  x y = fmap (Property . ModifyProp [x] . flip  mod) . getRecord y
    fieldQuot x y = fmap (Property . ModifyProp [x] . flip quot) . getRecord y
    fieldRem  x y = fmap (Property . ModifyProp [x] . flip  rem) . getRecord y

class (Monad m, FieldNum field m record f, Fractional f) => FieldFractional field m record f
  where
    (=/) :: (FieldGetA ~?= field, FieldModifyA ~?= field)
         => field -> field -> record -> m (Prop m field record)

instance (Monad m, Fractional f) => FieldFractional (FObject (FieldC m record f) as) m record f
  where
    x =/ y = fmap (Property . ModifyProp [x] . flip (/)) . getRecord y

