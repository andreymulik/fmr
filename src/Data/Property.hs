{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE Safe, GADTs, DefaultSignatures, KindSignatures, DataKinds, CPP #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

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
  IsProp (..), PropertyKind, FieldKind, Prop (..),
  
  -- ** Basic properties
  FieldGet (..), get, gets',
  FieldSet (..), set, sets',
  
  -- *** Set properties
  pattern (:=), pattern (::=), pattern (:=$), pattern (::=$),
  
  -- *** Monadic set properties
  pattern (:<=), pattern (:<=$), pattern (:=<), pattern (:=<$),
  
  -- ** Modify properties
  FieldModify (..),
  
  -- *** Modify properties
  pattern (:~), pattern (:~$), pattern (::~), pattern (::~$),
  
  -- *** Monadic modify properties
  pattern (:<~), pattern (:<~$), pattern (:~<), pattern (:~<$),
  
  -- ** Switch properties
  IsSwitch (..), FieldSwitch (..), switch, incr, decr
)
where

import Data.Typeable
import Data.Kind

import Control.Monad

default ()

--------------------------------------------------------------------------------

{- Generalized property. -}

{- |
  @since 0.2
  'Prop' is new, generalized and extensible property type (existential), which
  may contain any 'IsProp' value.
-}
data Prop m field record
  where
    Prop :: (Monad m, IsProp prop) => prop m field record -> Prop m field record
  deriving ( Typeable )

{- |
  @since 0.2
  'IsProp' is a property class that allows you to extend @fmr@ syntax. Now you
  can create new property types and use it with existing in 'set' list of
  actions.
-}
class IsProp (prop :: PropertyKind)
  where
    -- | @performProp record prop @ performs an action on @record@ using @prop@.
    performProp :: (Monad m) => record -> prop m field record -> m ()

instance IsProp SetProp
  where
    performProp record (SetRecordPropM field   kl) = setRecords field record =<< kl record
    performProp record (SetRecordProp  field    f) = setRecords field record (f record)
    performProp record (SetPropM       field mval) = setRecords field record =<< mval
    performProp record (SetProp        field  val) = setRecords field record val

setRecords :: (Monad m, FieldSet field) => [field m record a] -> record -> a -> m ()
setRecords fields record val = forM_ fields $ \ field -> setRecord field record val

instance IsProp ModifyProp
  where
    performProp record (Modify      field f) = () <$ modifyRecords  field record (f record)
    performProp record (ModifyM     field f) = () <$ modifyRecordsM field record (f record)
    performProp record (ModifyProp  field f) = () <$ modifyRecords  field record f
    performProp record (ModifyPropM field f) = () <$ modifyRecordsM field record f

modifyRecords :: (Monad m, FieldModify field) =>
  [field m record b] -> record -> (b -> b) -> m ()
modifyRecords fields record f = fields `forM_` \ field -> modifyRecord field record f

modifyRecordsM :: (Monad m, FieldModify field, FieldGet field) =>
  [field m record b] -> record -> (b -> m b) -> m ()
modifyRecordsM fields record f = fields `forM_` \ field -> modifyRecordM field record f

instance IsProp SwitchProp
  where
    performProp record (SwitchProp n field) = switchRecord field record n

--------------------------------------------------------------------------------

-- | @since 0.2 Service kind synonym.
type FieldKind = (Type -> Type) -> Type -> Type -> Type

-- | @since 0.2 Service kind synonym.
type PropertyKind = (Type -> Type) -> FieldKind -> Type -> Type

--------------------------------------------------------------------------------

{- fmr classes. -}

-- | @since 0.2 Property getter class.
class FieldGet field
  where
    -- | @'getRecord' field record@ return @record@'s value using @field@.
    getRecord :: (Monad m) => field m record a -> record -> m a

-- | @since 0.2 Property setter class.
class FieldSet field
  where
    -- | @'setRecord' field record value@ sets new @record@ @value@.
    setRecord :: (Monad m) => field m record a -> record -> a -> m ()

{- |
  @since 0.2
  Property modifier class.
  
  Note that 'FieldModifier' doesn't go well with write-only fields, because
  'modifyRecord' returns new value, and 'modifyRecordM' also assumes the
  possibility of old value \"leaking\" (hence the 'FieldGet' constraint is
  imposed on it).
-}
class (FieldSet field) => FieldModify field
  where
    {- |
      @'modifyRecord' field record upd@ modifies @record@ @field@ using @upd@.
      Returns new value.
    -}
    default modifyRecord :: (Monad m, FieldGet field) =>
      field m record a -> record -> (a -> a) -> m a
    modifyRecord :: (Monad m) => field m record a -> record -> (a -> a) -> m a
    modifyRecord field record f = do
      val <- f <$> get field record
      setRecord field record val
      return val
    
    {- |
      @'modifyRecordM' field record upd@ modifies @record@ @field@ using @upd@.
      Note that assumes the possibility of old value \"leaking\", e.g.:
      
      @
        -- get value using 'modifyRecordM'
        getLeak = do
          -- Some read-only fields
          x <- newWriteOnly
          y <- 'var' Nothing
          -- write current value to y and do not modify.
          modifyRecordM this x (\ val -> do set [this := Just val]; return val)
          -- leaking: return current value of "write-only" record
          get this y
      @
      
      So you cannot use it for write-only fields.
    -}
    modifyRecordM :: (Monad m, FieldGet field) =>
      field m record a -> record -> (a -> m a) -> m a
    modifyRecordM field record f = do
      val <- f =<< get field record
      setRecord field record val
      return val

{- |
  Switch property modifier.
  
  Note that 'FieldSwitch' is designed for deterministic switches with a
  pre-known set of states and order of transitions between them, without
  branches. If you need more complex behavior, use 'FieldModify'.
-}
class FieldSwitch field
  where
    -- | Repeated increment or decrement.
    switchRecord :: (Monad m, IsSwitch a) => field m record a -> record -> Int -> m ()

--------------------------------------------------------------------------------

{- fmr properties. -}

{- |
  @since 0.2
  'SetProp' is a service type used to set field values.
  See @(':=')@, @(':=$')@, @('::=')@, @('::=$')@, @(':<=')@, @(':<=$')@,
  @(':=<')@ and @(':=<$')@ patterns.
-}
data SetProp m field record
  where
    -- | 'SetProp' corresponds to @(':=$')@ and @(':=')@.
    SetProp :: (FieldSet field) =>
      [field m record a] -> a -> SetProp m field record
    
    -- | 'SetPropM' corresponds to @('::=$')@ and @('::=')@.
    SetPropM :: (FieldSet field) =>
      [field m record a] -> m a -> SetProp m field record
    
    -- | 'SetRecordProp' corresponds to @(':<=$')@ and @(':<=')@.
    SetRecordProp :: (FieldSet field) =>
      [field m record a] -> (record -> a) -> SetProp m field record
    
    -- | 'SetRecordPropM' corresponds to @(':=<$')@ and @(':=<')@.
    SetRecordPropM :: (FieldSet field) =>
      [field m record a] -> (record -> m a) -> SetProp m field record
  deriving ( Typeable )

{- |
  @since 0.2
  'ModifyProp' is a service type used to modify field values.
  See @(':~')@, @(':~$')@, @(':<~')@, @(':<~$')@, @('::~')@, @('::~$')@,
  @(':~<')@, @(':~<$')@ patterns.
-}
data ModifyProp m field record
  where
    -- | 'FieldModify' constructor corresponds to @(':~$')@ and @(':~')@.
    ModifyProp :: (FieldModify field) =>
      [field m record a] -> (a -> a) -> ModifyProp m field record
    
    -- | 'FieldModify' constructor corresponds to @(':<~$')@ and @(':<~')@.
    ModifyPropM :: (FieldModify field, FieldGet field) =>
      [field m record a] -> (a -> m a) -> ModifyProp m field record
    
    -- | 'Modify' constructor corresponds to @('::~$')@ and @('::~')@.
    Modify :: (FieldModify field) =>
      [field m record a] -> (record -> a -> a) -> ModifyProp m field record
    
    -- | 'ModifyM' constructor corresponds to @(':~<$')@ and @(':~<')@.
    ModifyM :: (FieldModify field, FieldGet field) =>
      [field m record a] -> (record -> a -> m a) -> ModifyProp m field record
  deriving ( Typeable )

{- |
  @since 0.2
  'SwitchProp' is a service type used to update record values, see 'switch',
  'incr' and 'decr'.
-}
data SwitchProp m field record
  where
    -- | Switch field, see 'switch', 'incr' and 'decr'.
    SwitchProp :: (Monad m, FieldSwitch field, IsSwitch a) =>
      Int -> field m record a -> SwitchProp m field record
  deriving ( Typeable )

--------------------------------------------------------------------------------

{- fmr pure setters. -}

{- |
  Pure value setter. @set record [field := value]@ set @value@ to @record@'s
  @field@.
-}
pattern (:=) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, FieldSet field
  ) => field m record a -> a -> Prop m field record
pattern field := val = [field] :=$ val

{- |
  Pure value setter with @record@. @set record [field ::= f]@ set @f record@ to
  @record@'s @field@.
  
  @
    set record [field ::= const val] === set record [field := val]
  @
-}
pattern (::=) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, FieldSet field
  ) => field m record a -> (record -> a) -> Prop m field record
pattern field ::= f = [field] ::=$ f

{- |
  @since 0.2
  Pure group setter. @set record [fields :=$ value]@ set @value@ to @record@'s
  some @fields@.
  
  @
    set record [[field] :=$ value] === set record [field := value]
  @
-}
pattern (:=$) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, FieldSet field
  ) => [field m record a] -> a -> Prop m field record
pattern field :=$ val <- (cast -> Just (SetProp field val))
  where
    (:=$) = Prop ... SetProp

{- |
  @since 0.2
  Pure group setter with @record@. @set record [fields ::=$ f]@ set @f record@
  to @record@'s some @fields@.
  
  @
    set record [[field] ::=$ f] === set record [field ::= f]
    set record [fields ::=$ const val] === set record [fields :=$ val]
  @
-}
pattern (::=$) ::
  (
    Typeable m, Typeable field, Typeable record, Monad m, FieldSet field
  ) => [field m record a] -> (record -> a) -> Prop m field record
pattern field ::=$ f <- (cast -> Just (SetRecordProp field f))
  where
    (::=$) = Prop ... SetRecordProp

--------------------------------------------------------------------------------

{- fmr pure updaters. -}

{- |
  Pure value modifier. @set record [field :~ f]@ modify value of @record@'s
  @field@ using @f@ function.
  
  @
    set record [field :~ const val] === set record [field := val]
  @
-}
pattern (:~) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, FieldModify field
  ) => field m record a -> (a -> a) -> Prop m field record
pattern field :~ f = [field] :~$ f

{- |
  Pure value modifier with @record@. @set record [field ::~ f]@ modify value of
  @record@'s @field@ using @f record@ function.
  
  @
    set record [field ::~ const f] === set record [field :~ f]
  @
-}
pattern (::~) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, FieldModify field
  ) => field m record a -> (record -> a -> a) -> Prop m field record
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
pattern (:~$) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, FieldModify field
  ) => [field m record a] -> (a -> a) -> Prop m field record
pattern field :~$ f <- (cast -> Just (ModifyProp field f))
  where
    (:~$) = Prop ... ModifyProp

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
pattern (::~$) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, FieldModify field
  ) => [field m record a] -> (record -> a -> a) -> Prop m field record
pattern field ::~$ f <- (cast -> Just (Modify field f))
  where
    (::~$) = Prop ... Modify

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
pattern (:<=) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, FieldSet field
  ) => field m record a -> m a -> Prop m field record
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
pattern (:=<) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, FieldSet field
  ) => field m record a -> (record -> m a) -> Prop m field record
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
pattern (:<=$) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, FieldSet field
  ) => [field m record a] -> m a -> Prop m field record
pattern field :<=$ mval <- (cast -> Just (SetPropM field mval))
  where
    (:<=$) = Prop ... SetPropM

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
pattern (:=<$) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, FieldSet field
  ) => [field m record a] -> (record -> m a) -> Prop m field record
pattern field :=<$ f <- (cast -> Just (SetRecordPropM field f))
  where
    (:=<$) = Prop ... SetRecordPropM

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
pattern (:<~) ::
  (
    Typeable m, Typeable field, Typeable record, Monad m,
    FieldModify field, FieldGet field
  ) => field m record a -> (a -> m a) -> Prop m field record
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
pattern (:~<) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, FieldModify field, FieldGet field
  ) => field m record a -> (record -> a -> m a) -> Prop m field record
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
pattern (:<~$) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, FieldModify field, FieldGet field
  ) => [field m record a] -> (a -> m a) -> Prop m field record
pattern field :<~$ f <- (cast -> Just (ModifyPropM field f))
  where
    (:<~$) = Prop ... ModifyPropM

{- |
  @since 0.2
  Monadic group modifier with @record@. @set record [fields :~<$ f]@ modifies
  values of @record@'s @fields@ using @f record@ procedure. Note that the
  @f record@ is called every time a @field@ value is assigned.
  
  @
    set record [field :~<$ const f] === set record [fields :<~$ f]
  @
-}
pattern (:~<$) ::
  (
    Typeable m, Typeable field, Typeable record,
    Monad m, FieldModify field, FieldGet field
  ) => [field m record a] -> (record -> a -> m a) -> Prop m field record
pattern field :~<$ f <- (cast -> Just (ModifyM field f))
  where
    (:~<$) = Prop ... ModifyM

--------------------------------------------------------------------------------

-- | Service class for switchable types.
class IsSwitch switch
  where
    {- |
      @'toggle' s n@ "toggles" the state represented by the value @s@ by @n@
      positions, for example:
      
      @
        toggle n False = even n
        toggle n  True = odd  n
        toggle n  1234 = 1239 + n
      @
    -}
    toggle :: Int -> switch -> switch

instance IsSwitch Bool
  where
    toggle n False = even n
    toggle n  True = odd  n

instance (Integral i) => IsSwitch i where toggle n i = fromIntegral n + i

--------------------------------------------------------------------------------

-- | The 'get' function reads current value of a field.
get :: (Monad m, FieldGet field) => field m record a -> record -> m a
get =  getRecord

-- | @'gets'' fields record@ returns list of @record@ @fields@ values.
gets' :: (Monad m, FieldGet field) => record -> [field m record a] -> m [a]
gets' =  mapM . flip getRecord

{- |
  'set' is the main function in @fmr@, which allows you to describe changing the
  value of a record as a sequence of operations on its fields, e.g.
-}
set :: (Monad m) => record -> [Prop m field record] -> m ()
set record = mapM_ $ \ (Prop prop) -> performProp record prop

-- | Just synonym for 'set'.
sets' :: (Monad m) => record -> [Prop m field record] -> m ()
sets' =  set

-- | 'switch' changes the value by n steps.
switch :: (Monad m, FieldSwitch field, IsSwitch a) =>
  field m record a -> Int -> Prop m field record
switch field n = Prop (SwitchProp n field)

-- | @'incr' field@ is same as @switch field 1@.
incr :: (Monad m, FieldSwitch field, IsSwitch a) =>
  field m record a -> Prop m field record
incr =  Prop . SwitchProp 1

-- | @'decr' field@ is same as @switch field (-1)@.
decr :: (Monad m, FieldSwitch field, IsSwitch a) =>
  field m record a -> Prop m field record
decr =  Prop . SwitchProp (-1)

--------------------------------------------------------------------------------

-- | @sdp@ @(.)@-like combinator.
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) =  (.) . (.)

