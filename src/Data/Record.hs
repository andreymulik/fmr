{-# LANGUAGE GADTs #-}

{- |
    License     :  BSD-style
    Module      :  Data.Record
    Copyright   :  (c) Andrey Mulik 2020
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.Record@ provides fake field type for record-style processing.
-}
module Data.Record
(
  -- * RecordField
  RecordField (..),
  
  -- * RecordProp
  RecordProp (..)
)
where

default ()

--------------------------------------------------------------------------------

-- | Class of abstract record fields.
class RecordField f
  where
    -- | Set new values to record's fields.
    set' :: (Monad m) => r -> [RecordProp f m r] -> m ()
    
    -- | Get value from record's field.
    get' :: (Monad m) => r -> f m r a -> m a

--------------------------------------------------------------------------------

-- | @'RecordProp' f m d@ is field @f@ that is already associated with a value.
data RecordProp f m r
  where
    (:=)  :: (Monad m) => f m r a ->       a       -> RecordProp f m r
    (:~)  :: (Monad m) => f m r a ->    (a -> a)   -> RecordProp f m r
    (::=) :: (Monad m) => f m r a ->    (r -> a)   -> RecordProp f m r
    (::~) :: (Monad m) => f m r a -> (r -> a -> a) -> RecordProp f m r



