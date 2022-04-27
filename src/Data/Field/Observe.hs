{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE Safe, DataKinds, TypeFamilies, TypeOperators, PatternSynonyms #-}

{- |
    License     :  BSD-style
    Module      :  Data.Field.Observe
    Copyright   :  (c) Andrey Mulik 2021-2022
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.Field.Observe@ simple field observer.
-}
module Data.Field.Observe
(
  -- * Exports
  module Data.Property,
  
  -- * Observe convention
  Observe (..),
  
  -- ** Observable fields
  MFieldObserverC (..), OGMField, OMField,
  FieldObserverC  (..), OGField,  OField,
  
  -- ** Event
  FieldEvent (..), EventC (..), getEvent, setEvent, modifyEvent, modifyMEvent,
  
  -- ** Notifier
  FieldNotify (..), Notifier (..)
)
where

import Data.Field.Object
import Data.Property
import Data.MField

import Data.Typeable
import Data.Kind

import GHC.TypeLits

import Control.Monad

default ()

--------------------------------------------------------------------------------

{- |
  @'Observe' m field@ is derivative convention, which use 'field' convention
  (e.g. 'FieldC') for accessor representation and 'EventC'-based
  conventions for representation of events.
-}
data Observe m field (a :: Symbol) = Observe
  {
    observeField  :: !(field a),
    observeEvent  :: !(EventC field a),
    observeAfter  :: !(Var m [Notifier m a]),
    observeBefore :: !(Var m [Notifier m a])
  } deriving ( Typeable )

{- |
  'EventC' is a convention that defines event types for 'FieldC'.
  You can also use it with your own accessors and conventions.
-}
data family EventC (for :: Symbol -> Type) (a :: Symbol) :: Type

-- | Accessor-specific notifier type.
newtype Notifier m (a :: Symbol) = Notify
  {
    -- | Get notifier procedure.
    notifier :: m ()
  } deriving ( Typeable )

--------------------------------------------------------------------------------

-- | 'FieldC'-based convention.
newtype FieldObserverC m record e a = FieldObserverC
  {
    fromFieldObserverC :: Observe m (FieldC m record e) a
  } deriving ( Typeable )

-- | Observable 'GField'.
type OGField as = GField as FieldObserverC

-- | Observable 'Field'.
type OField = OGField FieldA

instance (MonadVar m, FieldGetA ~:= as) => IsField (OGField as m record e)
                                            (FieldC m record e) FieldGetA
  where
    fromField f = FieldGetA $ \ record -> do
      let FieldObserverC (Observe g e a b) = fromField f
      mapM_ notifier =<< get this b
      
      val <- fieldGetA g record
      fieldGetE e record val
      
      mapM_ notifier =<< get this a
      return val

instance (MonadVar m, FieldSetA ~:= as) => IsField (OGField as m record e)
                                            (FieldC m record e) FieldSetA
  where
    fromField (GField f) = FieldSetA $ \ record val -> do
      let FieldObserverC (Observe s e a b) = fromField f
      mapM_ notifier =<< get this b
      
      fieldSetA s record val
      fieldSetE e record val
      
      mapM_ notifier =<< get this a

instance (MonadVar m, FieldModifyA ~:= as) => IsField (OGField as m record e)
                                            (FieldC m record e) FieldModifyA
  where
    fromField (GField f) = FieldModifyA $ \ record upd -> do
      let FieldObserverC (Observe m e a b) = fromField f
      mapM_ notifier =<< get this b
      
      val <- fieldModifyA m record upd
      fieldModifyE e record val
      
      mapM_ notifier =<< get this a
      return val

instance (MonadVar m, FieldModifyMA ~:= as) => IsField (OGField as m record e)
                                            (FieldC m record e) FieldModifyMA
  where
    fromField (GField f) = FieldModifyMA $ \ record go -> do
      let FieldObserverC (Observe mm e a b) = fromField f
      mapM_ notifier =<< get this b
      
      val <- fieldModifyMA mm record go
      fieldModifyME e record val
      
      mapM_ notifier =<< get this a
      return val

--------------------------------------------------------------------------------

-- | 'MFieldC'-based convention.
newtype MFieldObserverC m e a = MFieldObserverC
  {
    fromMFieldObserverC :: Observe m (MFieldC m e) a
  } deriving ( Typeable )

-- | Observable 'GMField'.
type OGMField as = GMField as MFieldObserverC

-- | Observable 'MField'.
type OMField = OGMField FieldA

instance (MonadVar m, FieldGetA ~:= as) => IsField (OGMField as m record e)
                                             (FieldC m record e) FieldGetA
  where
    fromField (GMField mref) = FieldGetA $ \ record -> do
      f <- get this $ fromGMFieldRef (mref record)
      let MFieldObserverC (Observe g e a b) = fromField f
      mapM_ notifier =<< get this b
      
      val <- join.get this $ mfieldGetA g
      mfieldGetE e val
      
      mapM_ notifier =<< get this a
      return val

instance (MonadVar m, FieldSetA ~:= as) => IsField (OGMField as m record e)
                                             (FieldC m record e) FieldSetA
  where
    fromField (GMField mref) = FieldSetA $ \ record val -> do
      f <- get this $ fromGMFieldRef (mref record)
      let MFieldObserverC (Observe s e a b) = fromField f
      mapM_ notifier =<< get this b
      
      ($ val) =<< get this (mfieldSetA s)
      mfieldSetE e val
      
      mapM_ notifier =<< get this a

instance (MonadVar m, FieldModifyA ~:= as) => IsField (OGMField as m record e)
                                             (FieldC m record e) FieldModifyA
  where
    fromField (GMField mref) = FieldModifyA $ \ record upd -> do
      f <- get this $ fromGMFieldRef (mref record)
      let MFieldObserverC (Observe m e a b) = fromField f
      mapM_ notifier =<< get this b
      
      val <- ($ upd) =<< get this (mfieldModifyA m)
      mfieldModifyE e val
      
      mapM_ notifier =<< get this a
      return val

instance (MonadVar m, FieldModifyMA ~:= as) => IsField (OGMField as m record e)
                                             (FieldC m record e) FieldModifyMA
  where
    fromField (GMField mref) = FieldModifyMA $ \ record go -> do
      f <- get this $ fromGMFieldRef (mref record)
      let MFieldObserverC (Observe mm e a b) = fromField f
      mapM_ notifier =<< get this b
      
      val <- ($ go) =<< get this (mfieldModifyMA mm)
      mfieldModifyME e val
      
      mapM_ notifier =<< get this a
      return val

--------------------------------------------------------------------------------

-- | Common get accessor event.
newtype instance EventC (FieldC m record e) FieldGetA =
    FieldGetE {fieldGetE :: EventGet m record e}
  deriving ( Typeable )

-- | Common get accessor event convention.
type EventGet m record e = record -> e -> m ()

-- | Common set accessor event.
newtype instance EventC (FieldC m record e) FieldSetA =
    FieldSetE {fieldSetE :: EventSet m record e}
  deriving ( Typeable )

-- | Common set accessor event convention.
type EventSet m record e = record -> e -> m ()

-- | Common modify accessor event.
newtype instance EventC (FieldC m record e) FieldModifyA =
    FieldModifyE {fieldModifyE :: EventModify m record e}
  deriving ( Typeable )

-- | Common modify accessor event convention.
type EventModify m record e = record -> e -> m ()

-- | Common monadic modify accessor event.
newtype instance EventC (FieldC m record e) FieldModifyMA =
    FieldModifyME {fieldModifyME :: EventModifyM m record e}
  deriving ( Typeable )

-- | Common monadic modify accessor event convention.
type EventModifyM m record e = record -> e -> m ()

--------------------------------------------------------------------------------

-- | Common get accessor event.
newtype instance EventC (MFieldC m e) FieldGetA =
    MFieldGetE {mfieldGetE :: EventMGet m e}
  deriving ( Typeable )

-- | Common get accessor event convention.
type EventMGet m e = e -> m ()

-- | Common set accessor event.
newtype instance EventC (MFieldC m e) FieldSetA =
    MFieldSetE {mfieldSetE :: EventMSet m e}
  deriving ( Typeable )

-- | Common set accessor event convention.
type EventMSet m e = e -> m ()

-- | Common modify accessor event.
newtype instance EventC (MFieldC m e) FieldModifyA =
    MFieldModifyE {mfieldModifyE :: EventMModify m e}
  deriving ( Typeable )

-- | Common modify accessor event convention.
type EventMModify m e = e -> m ()

-- | Common monadic modify accessor event.
newtype instance EventC (MFieldC m e) FieldModifyMA =
    MFieldModifyME {mfieldModifyME :: EventMModifyM m e}
  deriving ( Typeable )

-- | Common monadic modify accessor event convention.
type EventMModifyM m e = e -> m ()

--------------------------------------------------------------------------------

-- | Class of fields with public events.
class IsField field c a => FieldEvent field c (a :: Symbol) | field -> c
  where
    fromEvent :: field -> EventC c a

instance (IsField (OGField as m record e) (FieldC m record e) a, MonadVar m, a ~:= as)
    => FieldEvent (OGField as m record e) (FieldC m record e) a
  where
    fromEvent = observeEvent.fromFieldObserverC.fromField.fromGField

instance (MonadVar m, FieldGetA ~:= as) => FieldEvent (OGMField as m record e)
                                                (FieldC m record e) FieldGetA
  where
    fromEvent (GMField f) = FieldGetE . flip $ \ val -> get this.fromGMFieldRef.f
      >=> flip mfieldGetE val.observeEvent.fromMFieldObserverC.fromField

instance (MonadVar m, FieldSetA ~:= as) => FieldEvent (OGMField as m record e)
                                                (FieldC m record e) FieldSetA
  where
    fromEvent (GMField f) = FieldSetE . flip $ \ val -> get this.fromGMFieldRef.f
      >=> flip mfieldSetE val.observeEvent.fromMFieldObserverC.fromField

instance (MonadVar m, FieldModifyA ~:= as) => FieldEvent (OGMField as m record e)
                                                (FieldC m record e) FieldModifyA
  where
    fromEvent (GMField f) = FieldModifyE . flip $ \ val -> get this.fromGMFieldRef.f
      >=> flip mfieldModifyE val.observeEvent.fromMFieldObserverC.fromField

instance (MonadVar m, FieldModifyMA ~:= as) => FieldEvent (OGMField as m record e)
                                                (FieldC m record e) FieldModifyMA
  where
    fromEvent (GMField f) = FieldModifyME . flip $ \ val -> get this.fromGMFieldRef.f
      >=> flip mfieldModifyME val.observeEvent.fromMFieldObserverC.fromField

-- | Get @on-get@ event (in 'FieldC' convention).
getEvent :: FieldEvent field (FieldC m record e) FieldGetA
         => field -> record -> e -> m ()
getEvent =  fieldGetE.fromEvent

-- | Get @on-set@ event (in 'FieldC' convention).
setEvent :: FieldEvent field (FieldC m record e) FieldSetA
         => field -> record -> e -> m ()
setEvent =  fieldSetE.fromEvent

-- | Get @on-modify@ event (in 'FieldC' convention).
modifyEvent :: FieldEvent field (FieldC m record e) FieldModifyA
            => field -> record -> e -> m ()
modifyEvent =  fieldModifyE.fromEvent

-- | Get @on-modiffyM@ event (in 'FieldC' convention).
modifyMEvent :: FieldEvent field (FieldC m record e) FieldModifyMA
             => field -> record -> e -> m ()
modifyMEvent =  fieldModifyME.fromEvent

--------------------------------------------------------------------------------

-- | Class of fields with public notifiers.
class FieldNotify field a
  where
    notifyBefore :: MonadVar m => field m record e -> Field m record [Notifier m a]
    notifyAfter  :: MonadVar m => field m record e -> Field m record [Notifier m a]

instance a ~:= as => FieldNotify (OGField as) a
  where
    notifyBefore = subfield.const.observeBefore.fromFieldObserverC.fromField
    notifyAfter  = subfield.const.observeAfter.fromFieldObserverC.fromField

instance a ~:= as => FieldNotify (OGMField as) a
  where
    notifyBefore f = subfieldM $ fmap (observeBefore.fromMFieldObserverC.fromField)
                               . get this.fromGMFieldRef.fromGMField f
    
    notifyAfter  f = subfieldM $ fmap  (observeAfter.fromMFieldObserverC.fromField)
                               . get this.fromGMFieldRef.fromGMField f




