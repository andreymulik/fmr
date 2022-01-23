{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe, GADTs, TypeFamilies, TypeOperators, DataKinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

{- |
    License     :  BSD-style
    Module      :  Data.MField
    Copyright   :  (c) Andrey Mulik 2020-2021
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.MField@ provides mutable field type for record-style operations.
-}
module Data.MField
(
  -- * Exports
  module Data.Property,
  module Data.Field,
  
  -- * Mutable field
  GMField (..), MFieldC (..), getter, setter, modifier, modifierM,
  
  -- ** Mutable reference
  IsRef (..), GMFieldRef, MFieldRef, MFieldRep
)
where

import Data.Field.Object
import Data.Typeable
import Data.Property
import Data.Field
import Data.Kind

import GHC.TypeLits

import Control.Monad

default ()

--------------------------------------------------------------------------------

{- |
  'MFieldRef' is a structure containing accessors for a specific field of a
  specific record. As a rule, it makes sense to store 'MFieldRef' in the record
  to which it refers.
-}
type GMFieldRef m a = FObject (MFieldC m a)

getRef :: FieldGetA ~:= cs => GMFieldRef m a cs -> MGetter m a
getRef =  mFieldGetA.fromField

setRef :: FieldSetA ~:= cs => GMFieldRef m a cs -> MSetter m a
setRef =  mFieldSetA.fromField

modifyRef :: FieldModifyA ~:= cs => GMFieldRef m a cs -> MModifier m a
modifyRef =  mFieldModifyA.fromField

modifyMRef :: FieldModifyMA ~:= cs => GMFieldRef m a cs -> MModifierM m a
modifyMRef =  mFieldModifyMA.fromField

getter :: (FieldGetA ~:= cs, MonadVar m) => GMField m record a cs
       -> Field m record (m a)
getter (GMField f) =
  let Field g s m mm = this
  in  Field (g.getRef.f) (s.getRef.f) (m.getRef.f) (mm.getRef.f)

setter :: (FieldSetA ~:= cs, MonadVar m) => GMField m record a cs
       -> Field m record (a -> m ())
setter (GMField f) =
  let Field g s m mm = this
  in  Field (g.setRef.f) (s.setRef.f) (m.setRef.f) (mm.setRef.f)

modifier :: (FieldModifyA ~:= cs, MonadVar m) => GMField m record a cs
         -> Field m record ((a -> a) -> m a)
modifier (GMField f) =
  let Field g s m mm = this
  in  Field (g.modifyRef.f) (s.modifyRef.f) (m.modifyRef.f) (mm.modifyRef.f)

modifierM :: (FieldModifyMA ~:= cs, MonadVar m) => GMField m record a cs
          -> Field m record ((a -> m a) -> m a)
modifierM (GMField f) =
  let Field g s m mm = this
  in  Field (g.modifyMRef.f) (s.modifyMRef.f) (m.modifyMRef.f) (mm.modifyMRef.f)

--------------------------------------------------------------------------------

class IsRef ref m a | ref -> m, ref -> a
  where
    -- | Create new reference to the given variable.
    link :: (MonadVar m) => Var m a -> m ref
    
    -- | Get an associated 'MFieldRef' reference to the given reference.
    ref :: (MonadVar m) => ref -> MFieldRef m a

--------------------------------------------------------------------------------

-- | 'MFieldRep' is a helper type that stores a variable and its accessors.
data MFieldRep m a = MFieldRep !(Var m a) {-# UNPACK #-} !(MFieldRef m a)
  deriving ( Typeable )

instance IsRef (MFieldRep m a) m a
  where
    link x = MFieldRep x <$> link x
    ref (MFieldRep _ fr) = fr

--------------------------------------------------------------------------------

data family MFieldC (m :: Type -> Type) a (c :: Symbol) :: Type

newtype instance MFieldC m a FieldGetA = MFieldGetA
  {mFieldGetA :: MGetter m a}

type MGetter m a = Var m (m a)

newtype instance MFieldC m a FieldSetA = MFieldSetA
  {mFieldSetA :: MSetter m a}

type MSetter m a = Var m (a -> m ())

newtype instance MFieldC m a FieldModifyA = MFieldModifyA
  {mFieldModifyA :: MModifier m a}

type MModifier m a = Var m ((a -> a) -> m a)

newtype instance MFieldC m a FieldModifyMA = MFieldModifyMA
  {mFieldModifyMA :: MModifierM m a}

type MModifierM m a = Var m ((a -> m a) -> m a)

--------------------------------------------------------------------------------

{- |
  @since 0.2
  
  'MField' represents field with mutable value and accessors. Unlike fields of
  type 'Field', 'MField' fields are undesirable (and sometimes impossible) to be
  stored in the global environment. The dynamic nature of 'MField' means
  changing accessors for specific records, so accessors do not contain a record
  argument.
  
  'MField' isn't designed for accessing external abstract objects and interfaces,
  it doesn't give the same freedom as 'Field', but at the same time it allows
  you to create flexible connections between fields and values without having to
  reconcile them.
  
  'MField' contains a function that takes a record and returns an 'MFieldRef'.
  
  Example:
  
  @
    newtype HasField a = HasField {someField :: MFieldRep IO a}
    
    field :: MField IO (HasField a) a
    field =  MField (ref.someField)
    
    main :: IO ()
    main =  do
      putStrLn "Put some number"
      record <- fmap HasField $ link =<< var =<< readLn :: IO (HasField Integer)
      print =<< get field record
      
      set record [getter field :~ fmap (* 2)]
      print =<< get field record
  @
-}
newtype GMField m record a cs = GMField {fromGMField :: record -> GMFieldRef m a cs}
  deriving ( Typeable )

instance (MonadVar m, FieldGetA ~:= cs) => IsField (GMField m record e)
                                      (FieldC m record e) FieldGetA cs
  where
    fromField (GMField f) = FieldGetA (join.getField this.getRef.f)

instance (MonadVar m, FieldSetA ~:= cs) => IsField (GMField m record e)
                                      (FieldC m record e) FieldSetA cs
  where
    fromField (GMField f) = FieldSetA $ ap' (getField this.setRef.f)

instance (MonadVar m, FieldModifyA ~:= cs) => IsField (GMField m record e)
                                      (FieldC m record e) FieldModifyA cs
  where
    fromField (GMField f) = FieldModifyA $ ap' (getField this.modifyRef.f)

instance (MonadVar m, FieldModifyMA ~:= cs) => IsField (GMField m record e)
                                      (FieldC m record e) FieldModifyMA cs
  where
    fromField (GMField f) = FieldModifyMA $ ap' (getField this.modifyMRef.f)

--------------------------------------------------------------------------------

{- |
  'MFieldRef' is a structure containing accessors for a specific field of a
  specific record. As a rule, it makes sense to store 'MFieldRef' in the record
  to which it refers.
-}
type MFieldRef m a = GMFieldRef m a
  [FieldGetA, FieldSetA, FieldModifyA, FieldModifyMA]

instance IsRef (MFieldRef m a) m a
  where
    ref    = id
    link x = do
      let Field g s m mm = this
      
      g' <- var (g x); s' <- var (s x); m' <- var (m x); mm' <- var (mm x)
      return $ MFieldGetA    g' :++ MFieldSetA      s' :++
               MFieldModifyA m' :++ MFieldModifyMA mm' :++ FObjectEmpty

--------------------------------------------------------------------------------

ap' :: Monad m => (a -> m (b -> m c)) -> a -> b -> m c
ap' f = \ x y -> ($ y) =<< f x




