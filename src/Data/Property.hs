{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DefaultSignatures #-}
{-# LANGUAGE GADTs, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE Safe, DataKinds, KindSignatures #-}

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
  IsProp (..), FieldKind, Prop (..), set,
  
  -- ** Get property
  GetProp (..), get,
  
  -- ** Set property
  SetProp (..), pattern (:=), pattern (::=),
  
  -- ** Modify property
  ModifyProp (..), pattern (:~), pattern (::~),
  
  -- ** Switch property
  SwitchProp (..), switch, incr, decr
)
where

import Data.Typeable
import Data.Kind

default ()

--------------------------------------------------------------------------------

{- New-style, extensible property type. -}

{- |
  @since 0.2
  'Prop' is new style generalized property type, which may contain any 'IsProp'
  property.
  
  Unlike the @fmr-0.1@, this implementation isn't limited by predefined property
  syntax and can be easily extended by using the 'IsProp' class. However, now
  you can't extract information from the existing 'Prop'.
-}
data Prop m field record
  where
    Prop :: (IsProp prop field record) =>
      prop m field record -> Prop m field record
  deriving ( Typeable )

--------------------------------------------------------------------------------

{- New-style, generalized property definition. -}

{- |
  @since 0.2
  'IsProp' is a property class that allows you to extend @fmr@ syntax. Now you
  can create new property types and use it with existing in 'set'' list of
  actions.
-}
class IsProp prop (field :: FieldKind) record
  where
    -- | @performProp record prop @ performs an action on @record@ using @prop@.
    performProp :: (Monad m) => record -> prop m field record -> m ()

-- | @since 0.2 Service type synonym.
type FieldKind = (Type -> Type) -> Type -> Type -> Type

--------------------------------------------------------------------------------

{- Basic property classes. -}

-- | Class of fields types which supports value reading.
class GetProp field record
  where
    -- | @'getRecord' field record@ return @record@'s value using @field@.
    getRecord :: (Monad m) => field m record a -> record -> m a

-- | Property setter.
class SetProp field record
  where
    -- | @'setRecord' field record value@ sets new @record@ @value@.
    setRecord :: (Monad m) => field m record a -> record -> a -> m ()

-- | Property modifier.
class ModifyProp field record
  where
    -- | @'modifyRecord' field record upd@ modifies @record@ @field@ using @upd@.
    default modifyRecord :: (Monad m, GetProp field record, SetProp field record) =>
      field m record a -> record -> (a -> a) -> m a
    modifyRecord :: (Monad m) => field m record a -> record -> (a -> a) -> m a
    modifyRecord field record f = do
      val <- f <$> get field record
      setRecord field record val
      return val

-- | Switch property modifier.
class SwitchProp field a
  where
    -- | Repeated increment or decrement.
    switchRecord :: (Monad m) => field m record a -> record -> Int -> m ()

--------------------------------------------------------------------------------

{- Basic fields representation. -}

{- |
  @since 0.2
  'FieldSet' is a service type used to set field values, see @(':=')@ and
  @('::=')@.
-}
data FieldSet m field record
  where
    FieldSet :: (Monad m, SetProp field record) =>
      field m record a -> a -> FieldSet m field record
    
    RecordSet :: (Monad m, SetProp field record) =>
      field m record a -> (record -> a) -> FieldSet m field record
  deriving ( Typeable )

instance IsProp FieldSet field record
  where
    performProp record (FieldSet  field val) = setRecord field record val
    performProp record (RecordSet field   f) = setRecord field record (f record)

{- |
  @since 0.2
  'FieldModify' is a service type used to update record values, see @(':~')@ and
  @('::~')@.
-}
data FieldModify m field record
  where
    -- | 'FieldModify' corresponds to @(':~')@ 'Prop' constructor.
    FieldModify :: (Monad m, ModifyProp field record) =>
      field m record a -> (a -> a) -> FieldModify m field record
    
    -- | 'Modify' corresponds to @('::~')@ 'Prop' constructor.
    Modify :: (Monad m, ModifyProp field record) =>
      field m record a -> (record -> a -> a) -> FieldModify m field record
  deriving ( Typeable )

instance IsProp FieldModify field record
  where
    performProp record (FieldModify field f) = () <$ modifyRecord field record f
    performProp record (Modify      field f) = () <$ modifyRecord field record (f record)

{- |
  @since 0.2
  'FieldSwitch' is a service type used to update record values, see 'switch',
  'incr' and 'decr'.
-}
data FieldSwitch m field record
  where
    FieldSwitch :: (Monad m, SwitchProp field a) =>
      Int -> field m record a -> FieldSwitch m field record
  deriving ( Typeable )

instance IsProp FieldSwitch field record
  where
    performProp record (FieldSwitch n field) = switchRecord field record n

--------------------------------------------------------------------------------

{- Basic patterns. -}

-- | Set new value to field.
pattern (:=) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, SetProp field record
  ) => field m record a -> a -> Prop m field record
pattern field := val <- (cast -> Just (FieldSet field val)) where field := val = Prop (FieldSet field val)

-- | Update field value using current record value.
pattern (::=) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, SetProp field record
  ) => field m record a -> (record -> a) -> Prop m field record
pattern field ::= f <- (cast -> Just (RecordSet field f)) where field ::= f = Prop (RecordSet field f)

-- | Update field value using current field value.
pattern (:~) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, ModifyProp field record
  ) => field m record a -> (a -> a) -> Prop m field record
pattern field :~ f <- (cast -> Just (FieldModify field f))
  where
    field :~ f = Prop (FieldModify field f)

-- | Update field value using current record and field values.
pattern (::~) ::
  (
    Typeable m, Typeable field, Typeable record, 
    Monad m, ModifyProp field record
  ) => field m record a -> (record -> a -> a) -> Prop m field record
pattern field ::~ f <- (cast -> Just (Modify field f))
  where
    field ::~ f = Prop (Modify field f)

--------------------------------------------------------------------------------

{- Helpful functions. -}

{- |
  'set' is the main function in @fmr@, which allows you to describe changing the
  value of a record as a sequence of operations on its fields, e.g.
  
  @
  set record
    [
      field := value, -- set new @value@ to @field@
      field :~   upd, -- update @field@ by appying @upd@ function to current @record@ value
      field ::=  upd, -- update @field@ by applying @upd@ function to current @field@ value
      field ::~  upd, -- update @field@ by applying @upd@ to current @record@ and @field@ values
    ]
  @
-}
set :: (Monad m) => record -> [Prop m field record] -> m ()
set record = mapM_ $ \ (Prop prop) -> performProp record prop

-- | The 'get' function reads current value of a field.
get :: (Monad m, GetProp field record) => field m record a -> record -> m a
get =  getRecord

-- | 'switch' changes the value by n steps.
switch :: (Monad m, SwitchProp field a) => field m record a -> Int -> Prop m field record
switch field n = Prop (FieldSwitch n field)

-- | @'incr' field@ is same as @switch field 1@.
incr :: (Monad m, SwitchProp field a) => field m record a -> Prop m field record
incr =  Prop . FieldSwitch 1

-- | @'decr' field@ is same as @switch field (-1)@.
decr :: (Monad m, SwitchProp field a) => field m record a -> Prop m field record
decr =  Prop . FieldSwitch (-1)



