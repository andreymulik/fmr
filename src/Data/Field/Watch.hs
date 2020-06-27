{- |
    License     :  BSD-style
    Module      :  Data.Field.Watch
    Copyright   :  (c) Andrey Mulik 2020
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.Field.Watch@ provides reactive field type for record-style operations.
-}
module Data.Field.Watch
(
  -- * Export
  module Data.Record,
  
  -- * Reactive field and property
  WField, WProp, notifyGet, notifySet, notifyUpd,
  
  -- * Field tracking
  track, onVoid, onGet, onSet, onUpd,
  
  onWatch, onRead, onChange, onAccess
)
where

import Data.Record
import Data.Field

default ()

--------------------------------------------------------------------------------

-- | @'WProp' m d@ is 'WField' that is already associated with a value.
type WProp = RecordProp WField

--------------------------------------------------------------------------------

{- |
  'WField' is a reactive version of 'Field' with notifiers that are executed
  after each data access. 'WatchField' separates notifiers and data access and
  can replace them.
-}
data WField m d a = WatchFor
  {
    -- | Field for watch.
    access :: !(Field m d a),
    
    -- | Notify (about data access) after get.
    notifyGet :: !(d -> m ()),
    -- | Notify (about new value) after set.
    notifySet :: !(d -> a -> m ()),
    -- | Notify (about new value) after update.
    notifyUpd :: !(d -> a -> m ())
  }

--------------------------------------------------------------------------------

instance RecordField WField
  where
    get' d watch = do v <- getter (access watch) d; notifyGet watch d; return v
    
    set' d = mapM_ $ \ prop -> case prop of
      fld :=  v -> do setter (access fld) d v; notifySet fld d v
      fld :~  u -> do v <- modifier (access fld) d u; notifyUpd fld d v
      fld ::~ u -> do v <- modifier (access fld) d (u d); notifySet fld d v
      fld ::= u -> let v = u d in do setter (access fld) d v; notifySet fld d v

--------------------------------------------------------------------------------

{- Field tracking. -}

-- | Create new 'WField' without trackers.
track :: (Monad m) => Field m d a -> WField m d a
track f = let nw _ _ = return () in WatchFor f (\ _ -> return ()) nw nw

-- | Set 'getter'-only tracker.
onGet :: (Monad m) => (d -> m ()) -> WField m d a -> WField m d a
onGet wg f = f {notifyGet = wg}

-- | Set 'setter'-only tracker.
onSet :: (Monad m) => (d -> a -> m ()) -> WField m d a -> WField m d a
onSet ws f = f {notifySet = ws}

-- | Set 'modifier'-only tracker.
onUpd :: (Monad m) => (d -> a -> m ()) -> WField m d a -> WField m d a
onUpd wu f = f {notifyUpd = wu}

-- | Remove all field trackers.
onVoid :: (Monad m) => WField m d a -> WField m d a
onVoid =  track . access

-- | Track modification as a 'setter' and 'getter' composition.
onWatch :: (Monad m) => (d -> m ()) -> (d -> a -> m ()) -> WField m d a -> WField m d a
onWatch wg ws f = WatchFor (access f) wg ws (\ d v -> wg d >> ws d v)

-- | Track reading (by 'setter' and 'modifier').
onRead :: (Monad m) => (d -> m ()) -> WField m d a -> WField m d a
onRead wg f = f {notifyGet = wg, notifyUpd = \ d _ -> wg d}

-- | Track changing (by 'getter' and 'modifier').
onChange :: (Monad m) => (d -> a -> m ()) -> WField m d a -> WField m d a
onChange ws f = f {notifySet = ws, notifyUpd = ws}

-- | Track any access (by 'setter', 'getter' and 'modifier').
onAccess :: (Monad m) => (d -> m ()) -> WField m d a -> WField m d a
onAccess wa f = f {notifyGet = wa, notifySet = \ d _ -> wa d, notifyUpd = \ d _ -> wa d}

