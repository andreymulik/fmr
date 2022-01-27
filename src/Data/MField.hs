{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE Safe, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, DataKinds #-}

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
import Data.Property
import Data.Typeable
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
type GMFieldRef m e = FObject (MFieldC m e)

getRef :: FieldGetA ~:= as => GMFieldRef m e as -> MGetter m e
getRef =  mFieldGetA.fromField

setRef :: FieldSetA ~:= as => GMFieldRef m e as -> MSetter m e
setRef =  mFieldSetA.fromField

modifyRef :: FieldModifyA ~:= as => GMFieldRef m e as -> MModifier m e
modifyRef =  mFieldModifyA.fromField

modifyMRef :: FieldModifyMA ~:= as => GMFieldRef m e as -> MModifierM m e
modifyMRef =  mFieldModifyMA.fromField

getter :: (FieldGetA ~:= as, MonadVar m) => GMField m record e as
       -> Field m record (m e)
getter (GMField f) =
  let Field g s m mm = this
  in  Field (g.getRef.f) (s.getRef.f) (m.getRef.f) (mm.getRef.f)

setter :: (FieldSetA ~:= as, MonadVar m) => GMField m record e as
       -> Field m record (e -> m ())
setter (GMField f) =
  let Field g s m mm = this
  in  Field (g.setRef.f) (s.setRef.f) (m.setRef.f) (mm.setRef.f)

modifier :: (FieldModifyA ~:= as, MonadVar m) => GMField m record e as
         -> Field m record ((e -> e) -> m e)
modifier (GMField f) =
  let Field g s m mm = this
  in  Field (g.modifyRef.f) (s.modifyRef.f) (m.modifyRef.f) (mm.modifyRef.f)

modifierM :: (FieldModifyMA ~:= as, MonadVar m) => GMField m record e as
          -> Field m record ((e -> m e) -> m e)
modifierM (GMField f) =
  let Field g s m mm = this
  in  Field (g.modifyMRef.f) (s.modifyMRef.f) (m.modifyMRef.f) (mm.modifyMRef.f)

--------------------------------------------------------------------------------

class IsRef ref m e | ref -> m, ref -> e
  where
    -- | Create new reference to the given variable.
    link :: (MonadVar m) => Var m e -> m ref
    
    -- | Get an associated 'MFieldRef' reference to the given reference.
    ref :: (MonadVar m) => ref -> MFieldRef m e

--------------------------------------------------------------------------------

-- | 'MFieldRep' is a helper type that stores a variable and its accessors.
data MFieldRep m e = MFieldRep !(Var m e) !(MFieldRef m e)
  deriving ( Typeable )

instance IsRef (MFieldRep m e) m e
  where
    link x = MFieldRep x <$> link x
    ref (MFieldRep _ fr) = fr

--------------------------------------------------------------------------------

data family MFieldC (m :: Type -> Type) e (a :: Symbol) :: Type

newtype instance MFieldC m e FieldGetA = MFieldGetA
  {mFieldGetA :: MGetter m e}

type MGetter m e = Var m (m e)

newtype instance MFieldC m e FieldSetA = MFieldSetA
  {mFieldSetA :: MSetter m e}

type MSetter m e = Var m (e -> m ())

newtype instance MFieldC m e FieldModifyA = MFieldModifyA
  {mFieldModifyA :: MModifier m e}

type MModifier m e = Var m ((e -> e) -> m e)

newtype instance MFieldC m e FieldModifyMA = MFieldModifyMA
  {mFieldModifyMA :: MModifierM m e}

type MModifierM m e = Var m ((e -> m e) -> m e)

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
newtype GMField m record e as = GMField {fromGMField :: record -> GMFieldRef m e as}
  deriving ( Typeable )

instance MonadVar m => IsField (GMField m record e) (FieldC m record e) FieldGetA
  where
    fromField (GMField f) = FieldGetA (join.getField this.getRef.f)

instance MonadVar m => IsField (GMField m record e) (FieldC m record e) FieldSetA
  where
    fromField (GMField f) = FieldSetA $ ap' (getField this.setRef.f)

instance MonadVar m => IsField (GMField m record e) (FieldC m record e) FieldModifyA
  where
    fromField (GMField f) = FieldModifyA $ ap' (getField this.modifyRef.f)

instance MonadVar m => IsField (GMField m record e) (FieldC m record e) FieldModifyMA
  where
    fromField (GMField f) = FieldModifyMA $ ap' (getField this.modifyMRef.f)

--------------------------------------------------------------------------------

{- |
  'MFieldRef' is a structure containing accessors for a specific field of a
  specific record. As a rule, it makes sense to store 'MFieldRef' in the record
  to which it refers.
-}
type MFieldRef m e = GMFieldRef m e [FieldGetA, FieldSetA, FieldModifyA, FieldModifyMA]

instance IsRef (MFieldRef m e) m e
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




