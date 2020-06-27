{- |
    License     :  BSD-style
    Module      :  Data.Field
    Copyright   :  (c) Andrey Mulik 2020
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.Field@ provides fake field type for record-style operations.
-}
module Data.Field
(
  -- * Export
  module Data.Record,
  
  -- * Field and property
  Field (..), Prop,
  
  -- ** Field creation
  field, fieldGet, fieldModify,
  
  -- * Field protection
  Field', Prop', field'
)
where

import Data.Functor
import Data.Record

default ()

--------------------------------------------------------------------------------

-- | @'Prop' m d@ is 'Field' that is already associated with a value.
type Prop = RecordProp Field

-- | @'Prop'' m d@ is 'Field'' that is already associated with a value.
type Prop' = RecordProp Field'

--------------------------------------------------------------------------------

-- | Field provides the generalized interface for abstract field access.
data Field m d a = Field
  {
    -- | Return field value
    getter   :: d -> m a,
    -- | Set new field value
    setter   :: d -> a -> m (),
    -- | Apply function to old value and set it to field, return new value
    modifier :: d -> (a -> a) -> m a
  }

--------------------------------------------------------------------------------

instance RecordField Field
  where
    get' = flip getter
    
    set' d = mapM_ $ \ prop -> case prop of
      f :=  v -> void $ setter f d v
      f ::= u -> void $ setter f d (u d)
      f :~  u -> void $ modifier f d u
      f ::~ u -> void $ modifier f d (u d)

-- | Simple field, 'modifier' is 'getter' and 'setter' composition.
field :: (Monad m) => (d -> m a) -> (d -> a -> m ()) -> Field m d a
field s w = Field s w (\ d u -> do v <- s d; let v' = u v in v' <$ w d v')

-- | Read-only field. Note that 'setter' do nothing, but 'modifier' touch field.
fieldGet :: (Monad m) => (d -> m a) -> Field m d a
fieldGet w = Field w (\ _ _ -> return ()) (\ d _ -> w d)

-- | Modify-based field. Note what 'getter' writes to field.
fieldModify :: (Monad m) => (d -> (a -> a) -> m a) -> Field m d a
fieldModify m = Field (flip m id) (\ d -> void . m d . const) m

--------------------------------------------------------------------------------

-- | 'Field'' is protected 'Field'.
newtype Field' m d a = Field' (Field m d a)

field' :: Field m d a -> Field' m d a
field' =  Field'

instance RecordField Field'
  where
    get' d (Field' f) = get' d f
    
    set' d = mapM_ $ \ prop -> case prop of
      Field' f :=  v -> void $ setter f d v
      Field' f ::= u -> void $ setter f d (u d)
      Field' f :~  u -> void $ modifier f d u
      Field' f ::~ u -> void $ modifier f d (u d)




