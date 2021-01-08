{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    License     :  BSD-style
    Module      :  Data.Field
    Copyright   :  (c) Andrey Mulik 2020
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.Field@ provides fake field type for record-style operations.
-}
module Data.Field
(
  -- * Simple field
  SField (..), SOField,
  
  -- * Field
  Field (..), OField,
  
  -- * Observable field
  Observe (..), observe
)
where

import qualified Data.List as L

import Data.Property
import Data.Functor

default ()

infixr 0 ...

--------------------------------------------------------------------------------

-- | Simple field, which contain only getter and setter.
data SField m record a = SField
  {
    -- | Get field value
    getSField :: !(record -> m a),
    -- | Set field value
    setSField :: !(record -> a -> m ())
  }

-- | 'Observable' 'SField'.
type SOField = Observe SField

instance GetProp    SField record where getRecord = getSField
instance SetProp    SField record where setRecord = setSField
instance ModifyProp SField record

instance (SwitchProp Field record) => SwitchProp SField record
  where
    switchRecord n = switchRecord n . toField
    incRecord      = incRecord . toField
    decRecord      = decRecord . toField

instance (InsertProp Field record many) => InsertProp SField record many
  where
    prependRecord x = prependRecord x . toField
    appendRecord  x = appendRecord  x . toField

instance (DeleteProp Field record many) => DeleteProp SField record many
  where
    deleteRecord x = deleteRecord x . toField

toField :: (Monad m) => SField m record a -> Field m record a
toField sfield@(SField g s) = Field g s $ flip (`modifyRecord` sfield)

--------------------------------------------------------------------------------

-- | Normal field, which contain getter, setter and modifier.
data Field m record a = Field
  {
    -- | Get field value
    getField    :: !(record -> m a),
    -- | Set field value
    setField    :: !(record -> a -> m ()),
    -- | Modify field value
    modifyField :: !(record -> (a -> a) -> m a)
  }

-- | 'Observable' 'Field'.
type OField = Observe Field

instance GetProp    Field record where getRecord = getField
instance SetProp    Field record where setRecord = setField

instance ModifyProp Field record
  where
    modifyRecord upd field record = modifyField field record upd

instance (IsSwitch switch) => SwitchProp Field switch
  where
    incRecord = void ... modifyRecord switchInc
    decRecord = void ... modifyRecord switchDec
    
    switchRecord n = void ... modifyRecord (switch n)

instance InsertProp Field record []
  where
    appendRecord  = modifyRecord . (flip (++) . pure)
    prependRecord = modifyRecord . (:)

instance DeleteProp Field record []
  where
    deleteRecord = modifyRecord . L.delete

--------------------------------------------------------------------------------

-- | Simple field observer, which can run some handlers after each action.
data Observe field m record a = Observe
  {
    -- | Field to observe.
    observed :: field m record a,
    -- | 'set' observer
    onGet    :: record -> a -> m (),
    -- | 'set' observer
    onSet    :: record -> a -> m (),
    -- | ''switch', modify', 'prepend', 'append' and 'delete' observer
    onModify :: record -> m ()
  }

-- Create field with default observers.
observe :: (Monad m) => field m record a -> Observe field m record a
observe field =
  let nothing = \ _ _ -> return ()
  in  Observe field nothing nothing (\ _ -> return ())

instance (SwitchProp field a) => SwitchProp (Observe field) a
  where
    incRecord field record = do
      incRecord (observed field) record
      onModify field record
    
    decRecord field record = do
      decRecord (observed field) record
      onModify field record
    
    switchRecord n field record = do
      switchRecord n (observed field) record
      onModify field record

instance (GetProp field record) => GetProp (Observe field) record
  where
    getRecord field record = do
      res <- getRecord (observed field) record
      onGet field record res
      return res

instance (SetProp field record) => SetProp (Observe field) record
  where
    setRecord field record val = do
      setRecord (observed field) record val
      onSet field record val

instance (ModifyProp field record) => ModifyProp (Observe field) record
  where
    modifyRecord upd field record = do
      res <- modifyRecord upd (observed field) record
      onModify field record
      return res

instance (InsertProp field record many) => InsertProp (Observe field) record many
  where
    prependRecord x field record = do
      res <- prependRecord x (observed field) record
      onModify field record
      return res
    
    appendRecord x field record = do
      res <- appendRecord x (observed field) record
      onModify field record
      return res

instance (DeleteProp field record many) => DeleteProp (Observe field) record many
  where
    deleteRecord x field record = do
      res <- deleteRecord x (observed field) record
      onModify field record
      return res

--------------------------------------------------------------------------------

(...) :: (d -> c) -> (a -> b -> d) -> a -> b -> c
(...) =  (.) . (.)




