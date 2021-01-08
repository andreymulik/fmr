{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, BangPatterns #-}
{-# LANGUAGE DefaultSignatures, GADTs #-}

{- |
    License     :  BSD-style
    Module      :  Data.Property
    Copyright   :  (c) Andrey Mulik 2020
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.Property@ provides property type 'Prop' for record-style operations.
-}
module Data.Property
(
  -- * Property
  Prop (..), get, set, setProp,
  
  -- ** Get
  GetProp (..),
  
  -- ** Set
  SetProp (..),
  
  -- ** Modify
  ModifyProp (..), InsertProp (..), DeleteProp (..),
  
  -- *** Switch
  SwitchProp (..), IsSwitch (..)
)
where

import Data.Functor

default ()

--------------------------------------------------------------------------------

-- | Property representation
data Prop m field record
  where
    -- | Increase property value (switch flag, increment, etc.).
    Incr :: (Monad m, SwitchProp field a) =>
      field m record a -> Prop m field record
    
    -- | Decrease property value (switch flag, decrement, etc.).
    Decr :: (Monad m, SwitchProp field a) =>
      field m record a -> Prop m field record
    
    -- | Increase/decrease value many times.
    Switch :: (Monad m, SwitchProp field a) =>
      Int -> field m record a -> Prop m field record
    
    -- | @field ':=' val@ sets new value @val@ to record.
    (:=)  :: (Monad m, SetProp field record) =>
      field m record a -> a -> Prop m field record
    
    -- | @field '::=' upd@ - updates value using @upd@.
    (::=) :: (Monad m, SetProp field record) =>
      field m record a -> (record -> a) -> Prop m field record
    
    -- | @field ':~' upd@ updates value using @upd@.
    (:~)  :: (Monad m, ModifyProp field record) =>
      field m record a -> (a -> a) -> Prop m field record
    
    -- | @field '::=' upd@ - updates value using @upd@.
    (::~) :: (Monad m, ModifyProp field record) =>
      field m record a -> (record -> a -> a) -> Prop m field record
    
    -- | @field ':=+' val@ appends @val@ to @record@ value.
    (:=+) :: (Monad m, InsertProp field record many) =>
      field m record (many a) -> a -> Prop m field record
    
    -- | @val ':=+' field@ prepends @val@ to @record@ value.
    (:+=) :: (Monad m, InsertProp field record many) =>
      a -> field m record (many a) -> Prop m field record
    
    -- | @field ':~=' val@ removes @val@ from @record@ value
    (:~=) :: (Monad m, DeleteProp field record many, Eq a) =>
      field m record (many a) -> a -> Prop m field record

--------------------------------------------------------------------------------

-- | 'getRecord' shortcut.
get :: (Monad m, GetProp field record) =>
  field m record a -> record -> m a
get =  getRecord

-- | 'setProp' shortcut.
set :: (Monad m) => record -> [Prop m field record] -> m ()
set =  setProp

-- | @setRecord record props@ changes @record@ value using @props@ properties.
setProp :: (Monad m) => record -> [Prop m field record] -> m ()
setProp record = mapM_ $ \ prop -> case prop of
  Incr      field -> incRecord      field record
  Decr      field -> decRecord      field record
  Switch  n field -> switchRecord n field record
  
  field ::=   upd -> setRecord field record (upd record)
  field :=    val -> setRecord field record val
  field ::~   upd -> void $ modifyRecord (upd record) field record
  field :~    upd -> void $ modifyRecord upd field record
  
  field :=+   val -> void $ prependRecord val field record
  val   :+= field -> void $ appendRecord  val field record
  field :~=   val -> void $ deleteRecord  val field record

--------------------------------------------------------------------------------

-- | Property getter.
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
      (a -> a) -> field m record a -> record -> m a
    modifyRecord :: (Monad m) => (a -> a) -> field m record a -> record -> m a
    modifyRecord f field record = do
      old <- get field record
      let new = f old
      setRecord field record new
      return new

--------------------------------------------------------------------------------

-- | Switch property setter.
class SwitchProp field a
  where
    -- | Generalized increment.
    incRecord :: (Monad m) => field m record a -> record -> m ()
    
    -- | Generalized decrement.
    decRecord :: (Monad m) => field m record a -> record -> m ()
    
    -- | Increment/decrement many times.
    switchRecord :: (Monad m) => Int -> field m record a -> record -> m ()

-- | Prepend/append modifier.
class InsertProp field record many
  where
    -- | Prepends new element to existing value.
    prependRecord :: (Monad m) => a -> field m record (many a) -> record -> m (many a)
    
    -- | Appends new element to existing value.
    appendRecord :: (Monad m) => a -> field m record (many a) -> record -> m (many a)

-- | Delete modifier.
class DeleteProp field record many
  where
    -- | Delete element from value (if any).
    deleteRecord :: (Monad m, Eq a) => a -> field m record (many a) -> record -> m (many a)

--------------------------------------------------------------------------------

-- | Class of value that can be increased/decreased.
class IsSwitch switch
  where
    -- | Increase value.
    switchInc :: switch -> switch
    
    -- | Decrease value.
    switchDec :: switch -> switch
    
    -- | Increase/decrease value many times.
    switch :: Int -> switch -> switch
    switch n !x = case n `compare` 0 of
      GT -> switch (n - 1) (switchInc x)
      LT -> switch (n + 1) (switchDec x)
      EQ -> x

--------------------------------------------------------------------------------

instance {-# INCOHERENT #-} IsSwitch Bool
  where
    switch n x = even n == x -- switch n x = even n ? x $ not x
    switchInc  = not
    switchDec  = not

instance (Integral a) => IsSwitch a
  where
    switch n x = x + fromIntegral n
    switchInc  = succ
    switchDec  = pred

