{-# LANGUAGE Safe, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, PatternSynonyms #-}

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

data ObserverFor event field m (c :: Symbol) = ObserverFor
    (field c)              -- action
    (Var m (event      c)) -- event, runs after action
    (Var m [Notifier m c]) -- notifications before action
    (Var m [Notifier m c]) -- notifications after event
  deriving ( Typeable )

newtype GObserver event field m cs = GObserver
  {fromGObserve :: FObject (ObserverFor event field m) cs}

type Observer field m record e = GObserver (EventC m record e) (field m record e) m

--------------------------------------------------------------------------------

newtype Notifier m (a :: Symbol) = Notify {notifier :: m ()}
  deriving ( Typeable )

notify :: m () -> Notifier m a
notify =  Notify

--------------------------------------------------------------------------------

data family EventC (m :: Type -> Type) record e (a :: Symbol) :: Type

--------------------------------------------------------------------------------

newtype instance EventC m record e FieldGetA =
    FieldGetE {fieldGetE :: EventGet m record e}
  deriving ( Typeable )

type EventGet m record e = record -> e -> m ()

newtype instance EventC m record e FieldSetA =
    FieldSetE {fieldSetE :: EventSet m record e}
  deriving ( Typeable )

type EventSet m record e = record -> e -> m ()

newtype instance EventC m record e FieldModifyA =
    FieldModifyE {fieldModifyE :: EventModify m record e}
  deriving ( Typeable )

type EventModify m record e = record -> e -> m ()

newtype instance EventC m record e FieldModifyMA =
    FieldModifyMC {fieldModifyME :: EventModifyM m record e}
  deriving ( Typeable )

type EventModifyM m record e = record -> e -> m ()

--------------------------------------------------------------------------------

instance MonadVar m => IsField (Observer FieldC m record e) (FieldC m record e) FieldGetA
  where
    fromField (GObserver f) = FieldGetA $ \ record -> do
      let ObserverFor g e b a = fromField f
      mapM_ notifier =<< getField this b
      
      evt <- fieldGetE <$> getField this e
      val <- fieldGetA g record
      evt record val
      
      mapM_ notifier =<< getField this a
      return val

instance MonadVar m => IsField (Observer FieldC m record e) (FieldC m record e) FieldSetA
  where
    fromField (GObserver f) = FieldSetA $ \ record val -> do
      let ObserverFor s e b a = fromField f
      mapM_ notifier =<< getField this b
      
      fieldSetA s record val
      
      evt <- fieldSetE <$> getField this e
      evt record val
      
      mapM_ notifier =<< getField this a

instance MonadVar m => IsField (Observer FieldC m record e) (FieldC m record e) FieldModifyA
  where
    fromField (GObserver f) = FieldModifyA $ \ record f' -> do
        let ObserverFor m e b a = fromField f
        mapM_ notifier =<< getField this b
        
        evt <- fieldModifyE <$> getField this e
        val <- fieldModifyA m record f'
        evt record val
        
        mapM_ notifier =<< getField this a
        return val

instance MonadVar m => IsField (Observer FieldC m record e) (FieldC m record e) FieldModifyMA
  where
    fromField (GObserver f) = FieldModifyMA $ \ record go -> do
        let ObserverFor mm e b a = fromField f
        mapM_ notifier =<< getField this b
        
        evt <- fieldModifyME <$> getField this e
        val <- fieldModifyMA mm record go
        evt record val
        
        mapM_ notifier =<< getField this a
        return val

--------------------------------------------------------------------------------

{-# COMPLETE Observer #-}

pattern Observer :: c ~:= cs
  => Var m [Notifier m c] -> Var m (event c) -> Var m [Notifier m c]
  -> GObserver event field m cs
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




