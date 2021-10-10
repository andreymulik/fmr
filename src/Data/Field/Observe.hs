{- |
    License     :  BSD-style
    Module      :  Data.Field.Observe
    Copyright   :  (c) Andrey Mulik 2021
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.Field.Observe@ simple field observer.
-}
module Data.Field.Observe
(
  -- * Exports
  module Data.Property,
  
  -- * Observable field
  Observe (..), observe
)
where

import Data.Typeable
import Data.Property

default ()

--------------------------------------------------------------------------------

-- | Simple field observer, which can run some handlers after each action.
data Observe field m record a = Observe
  {
    -- | Field to observe.
    observed :: field m record a,
    -- | 'getRecord' observer
    onGet    :: record -> a -> m (),
    -- | 'setRecord' observer
    onSet    :: record -> a -> m (),
    -- | 'modifyRecord' observer
    onModify :: record -> m ()
  }
  deriving ( Typeable )

-- | Create field with default observers.
observe :: (Monad m) => field m record a -> Observe field m record a
observe field =
  let og = \ _ _ -> return (); om = \ _ -> return (); os = og
  in  Observe field og os om

--------------------------------------------------------------------------------

instance (FieldSwitch field) => FieldSwitch (Observe field)
  where
    switchRecord field record n = do
      switchRecord (observed field) record n
      onModify field record

instance (FieldGet field) => FieldGet (Observe field)
  where
    getRecord field record = do
      res <- getRecord (observed field) record
      onGet field record res
      return res

instance (FieldSet field) => FieldSet (Observe field)
  where
    setRecord field record val = do
      setRecord (observed field) record val
      onSet field record val

instance (FieldModify field, FieldGet field) => FieldModify (Observe field)
  where
    modifyRecord field record upd = do
      res <- modifyRecord (observed field) record upd
      onModify field record
      return res
    
    modifyRecordM field record upd = do
      res <- modifyRecordM (observed field) record upd
      onModify field record
      return res



