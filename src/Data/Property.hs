{-# LANGUAGE Safe, DefaultSignatures, GADTs, UndecidableInstances, BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

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
  ModifyProp (..), InsertProp (..), DeleteProp (..), SwitchProp (..)
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
      field m record a -> Int -> Prop m field record
    
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
get :: (Monad m, GetProp field record) => field m record a -> record -> m a
get =  getRecord

-- | 'setProp' shortcut.
set :: (Monad m) => record -> [Prop m field record] -> m ()
set =  setProp

-- | @setRecord record props@ changes @record@ value using @props@ properties.
setProp :: (Monad m) => record -> [Prop m field record] -> m ()
setProp record = mapM_ $ \ prop -> case prop of
  Incr    field   -> incRecord    field record
  Decr    field   -> decRecord    field record
  Switch  field n -> switchRecord field record n
  
  field ::=   upd -> setRecord field record (upd record)
  field :=    val -> setRecord field record val
  field ::~   upd -> void $ modifyRecord field record (upd record)
  field :~    upd -> void $ modifyRecord field record upd
  
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
      field m record a -> record -> (a -> a) -> m a
    modifyRecord :: (Monad m) => field m record a -> record -> (a -> a) -> m a
    modifyRecord field record f = do
      old <- get field record
      let new = f old
      setRecord field record new
      return new

--------------------------------------------------------------------------------

-- | Switch property modifier.
class SwitchProp field a
  where
    -- | Generalized increment, same as @switchRecord@.
    incRecord :: (Monad m) => field m record a -> record -> m ()
    
    -- | Generalized decrement.
    decRecord :: (Monad m) => field m record a -> record -> m ()
    
    {- |
      Increment/decrement many times.
      
      > decRecord field record = switchRecord field record (-1)
      > incRecord field record = switchRecord field record 1
    -}
    switchRecord :: (Monad m) => field m record a -> record -> Int -> m ()

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



