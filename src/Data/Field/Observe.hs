{-# LANGUAGE Safe, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, PatternSynonyms #-}

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
  
  -- * Observable field
  GObserver ( .., Observer, runEvent, notifyBefore, notifyAfter ),
  Observer, EventC (..), event', notifyBefore', notifyAfter',
  
  -- ** Observer
  ObserverFor (..),
  
  -- ** Notifier
  Notifier, notify, notifier
)
where

import Data.Field.Object
import Data.Property
import Data.Field

import Data.Typeable
import Data.Kind

import GHC.TypeLits

default ()

--------------------------------------------------------------------------------

-- | 'ObserverFor' contains an accessor, an event, and notifiers.
data ObserverFor event field m (c :: Symbol) = ObserverFor
    !(field c)              -- action
    !(Var m (event      c)) -- event, runs after action
    !(Var m [Notifier m c]) -- notifications before action
    !(Var m [Notifier m c]) -- notifications after event
  deriving ( Typeable )

{- |
  'GObserver' is a generic field containing accessors with notifiers and generic
  events.
-}
newtype GObserver event field m cs = GObserver
  {fromGObserve :: FObject (ObserverFor event field m) cs}

-- | An 'Observer' is an 'GObserver' with events defined in 'EventC' type family.
type Observer field m record e = GObserver (EventC m record e) (field m record e) m

--------------------------------------------------------------------------------

-- | Notifier type.
newtype Notifier m (a :: Symbol) = Notify
  {
    -- | Get notifier procedure.
    notifier :: m ()
  } deriving ( Typeable )

-- Create notifier from procedure.
notify :: Monad m => m () -> Notifier m a
notify =  Notify

--------------------------------------------------------------------------------

{- |
  'EventC' is a convention that defines event types for 'FieldC'.
  You can also use it with your own accessors and conventions.
-}
data family EventC (m :: Type -> Type) record e (a :: Symbol) :: Type

--------------------------------------------------------------------------------

-- | Common get accessor event.
newtype instance EventC m record e FieldGetA =
    FieldGetE {fieldGetE :: EventGet m record e}
  deriving ( Typeable )

-- | Common get accessor event convention.
type EventGet m record e = record -> e -> m ()

-- | Common set accessor event.
newtype instance EventC m record e FieldSetA =
    FieldSetE {fieldSetE :: EventSet m record e}
  deriving ( Typeable )

-- | Common set accessor event convention.
type EventSet m record e = record -> e -> m ()

-- | Common modify accessor event.
newtype instance EventC m record e FieldModifyA =
    FieldModifyE {fieldModifyE :: EventModify m record e}
  deriving ( Typeable )

-- | Common modify accessor event convention.
type EventModify m record e = record -> e -> m ()

-- | Common monadic modify accessor event.
newtype instance EventC m record e FieldModifyMA =
    FieldModifyMC {fieldModifyME :: EventModifyM m record e}
  deriving ( Typeable )

-- | Common monadic modify accessor event convention.
type EventModifyM m record e = record -> e -> m ()

--------------------------------------------------------------------------------

instance MonadVar m => IsField (Observer FieldC m record e as) (FieldC m record e) FieldGetA
  where
    type FieldGetA ~?= Observer FieldC m record e as = FieldGetA ~:= as
    
    fromField (GObserver f) = FieldGetA $ \ record -> do
      let ObserverFor g e b a = fromField f
      mapM_ notifier =<< get this b
      
      evt <- fieldGetE <$> get this e
      val <- fieldGetA g record
      evt record val
      
      mapM_ notifier =<< get this a
      return val

instance MonadVar m => IsField (Observer FieldC m record e as) (FieldC m record e) FieldSetA
  where
    type FieldSetA ~?= Observer FieldC m record e as = FieldSetA ~:= as
    
    fromField (GObserver f) = FieldSetA $ \ record val -> do
      let ObserverFor s e b a = fromField f
      mapM_ notifier =<< get this b
      
      fieldSetA s record val
      
      evt <- fieldSetE <$> get this e
      evt record val
      
      mapM_ notifier =<< get this a

instance MonadVar m => IsField (Observer FieldC m record e as) (FieldC m record e) FieldModifyA
  where
    type FieldModifyA ~?= Observer FieldC m record e as = FieldModifyA ~:= as
    
    fromField (GObserver f) = FieldModifyA $ \ record f' -> do
        let ObserverFor m e b a = fromField f
        mapM_ notifier =<< get this b
        
        evt <- fieldModifyE <$> get this e
        val <- fieldModifyA m record f'
        evt record val
        
        mapM_ notifier =<< get this a
        return val

instance MonadVar m => IsField (Observer FieldC m record e as) (FieldC m record e) FieldModifyMA
  where
    type FieldModifyMA ~?= Observer FieldC m record e as = FieldModifyMA ~:= as
    
    fromField (GObserver f) = FieldModifyMA $ \ record go -> do
        let ObserverFor mm e b a = fromField f
        mapM_ notifier =<< get this b
        
        evt <- fieldModifyME <$> get this e
        val <- fieldModifyMA mm record go
        evt record val
        
        mapM_ notifier =<< get this a
        return val

--------------------------------------------------------------------------------

{-# COMPLETE Observer #-}

pattern Observer :: c ~:= cs => Var m [Notifier m c] -> Var m (event c)
                             -> Var m [Notifier m c] -> GObserver event field m cs
pattern Observer{notifyBefore, runEvent, notifyAfter} <-
  GObserver (FObjectElem (ObserverFor _ runEvent notifyBefore notifyAfter))

--------------------------------------------------------------------------------

event' :: (MonadVar m, c ~:= cs) => Field m (GObserver event field m cs) (event c)
event' =  Field
  (\ (Observer _ e _) -> getField    this e) (\ (Observer _ e _) -> setField     this e)
  (\ (Observer _ e _) -> modifyField this e) (\ (Observer _ e _) -> modifyFieldM this e)

notifyBefore' :: (MonadVar m, c ~:= cs) => Field m (GObserver event field m cs) [Notifier m c]
notifyBefore' =  Field
  (\ (Observer b _ _) -> getField    this b) (\ (Observer b _ _) -> setField     this b)
  (\ (Observer b _ _) -> modifyField this b) (\ (Observer b _ _) -> modifyFieldM this b)

notifyAfter' :: (MonadVar m, c ~:= cs) => Field m (GObserver event field m cs) [Notifier m c]
notifyAfter' =  Field
  (\ (Observer _ _ a) -> getField    this a) (\ (Observer _ _ a) -> setField     this a)
  (\ (Observer _ _ a) -> modifyField this a) (\ (Observer _ _ a) -> modifyFieldM this a)




