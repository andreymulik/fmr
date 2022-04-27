{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe, UndecidableSuperClasses, FlexibleContexts, DataKinds #-}
{-# LANGUAGE GADTs, TypeFamilies, TypeOperators #-}

{- |
    License     :  BSD-style
    Module      :  Data.MField
    Copyright   :  (c) Andrey Mulik 2020-2022
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.MField@ provides mutable field type for record-style operations.
-}
module Data.MField
(
  -- * Exports
  module Data.Field,
  
  -- * Mutable field
  GMField (..), MField, MFieldC (..), (=:),
  MGetter, MSetter, MModifier, MModifierM,
  getter, setter, modifier, modifierM,
  
  -- ** Mutable reference
  IsRef (..), GMFieldRef (..), MFieldRef, MFieldRep, ref', mfieldRef, accessorM
)
where

import Data.Field.Object
import Data.Property
import Data.Typeable
import Data.Field
import Data.Kind

import GHC.TypeLits

import Control.Monad ( join, (<=<) )

default ()

--------------------------------------------------------------------------------

{- |
  'MFieldRef' is a structure containing accessors for a specific field of a
  specific record. As a rule, it makes sense to store 'MFieldRef' in the record
  to which it refers.
-}
newtype GMFieldRef as mfieldC m e = GMFieldRef
  {
    fromGMFieldRef :: Var m (FObject as (mfieldC m e))
  } deriving ( Typeable )

{- |
  'MFieldRef' is a structure containing accessors for a specific field of a
  specific record. As a rule, it makes sense to store 'MFieldRef' in the record
  to which it refers.
-}
type MFieldRef = GMFieldRef FieldA MFieldC

mfieldRef :: (MonadVar m, a ~:= as) => GMFieldRef as mfieldC m e -> m (mfieldC m e a)
mfieldRef =  fmap fromField.get this.fromGMFieldRef

accessorM :: (a ~:= as, MonadVar m) => (mfieldC m e a -> Var m e')
                                    -> GMField as mfieldC m record e
                                    -> Field m record e'
accessorM g (GMField f) = Field (getRecord     this.g <=< mfieldRef.f)
            (flip $ \ x -> ($ x).setRecord     this.g <=< mfieldRef.f)
            (flip $ \ x -> ($ x).modifyRecord  this.g <=< mfieldRef.f)
            (flip $ \ x -> ($ x).modifyRecordM this.g <=< mfieldRef.f)

--------------------------------------------------------------------------------

-- | 'MFieldRep' is a helper type that stores a variable and its accessors.
data GMFieldRep as m e = GMFieldRep !(Var m e) !(GMFieldRef as MFieldC m e)
  deriving ( Typeable )

-- | 'GMFieldRep' synonym with getter, setter and modifiers.
type MFieldRep = GMFieldRep FieldA

-- | Mutable field convention.
data family MFieldC (m :: Type -> Type) e (a :: Symbol) :: Type

--------------------------------------------------------------------------------

-- | Class of variable field accessors.
class MonadVar m => IsRef ref m e | ref -> m, ref -> e
  where
    -- | Get an associated 'MFieldRef' reference to the given reference.
    ref :: ref -> MFieldRef m e
    
    -- | Create new reference to the given variable.
    link :: Var m e -> m ref

instance MonadVar m => IsRef (MFieldRef m e) m e
  where
    ref    = id
    link x = let Field g s m mm = this in do
      g' <- MFieldGetA    <$> var (g x); s'  <- MFieldSetA     <$> var (s  x)
      m' <- MFieldModifyA <$> var (m x); mm' <- MFieldModifyMA <$> var (mm x)
      GMFieldRef <$> var (g' :++ s' :++ m' :++ mm' :++ FObjectEmpty)

instance MonadVar m => IsRef (MFieldRep m e) m e
  where
    ref (GMFieldRep _ fr) = fr
    link x = GMFieldRep x <$> link x

ref' :: (IsRef ref m e, MonadVar m) => e -> m ref
ref' =  link <=< var

--------------------------------------------------------------------------------

newtype instance MFieldC m e FieldGetA = MFieldGetA
  {
    mfieldGetA :: Var m (MGetter m e)
  } deriving ( Typeable )

-- | Mutable field getter.
type MGetter m e = m e

-- | 'GMField' mutable getter.
getter :: (MonadVar m, FieldGetA ~:= as) => GMField as MFieldC m record e
                                         -> Field m record (MGetter m e)
getter =  accessorM mfieldGetA

--------------------------------------------------------------------------------

newtype instance MFieldC m e FieldSetA = MFieldSetA
  {
    mfieldSetA :: Var m (MSetter m e)
  } deriving ( Typeable )

-- | Mutable field setter.
type MSetter m e = e -> m ()

-- | 'GMField' mutable setter.
setter :: (MonadVar m, FieldSetA ~:= as) => GMField as MFieldC m record e
                                         -> Field m record (MSetter m e)
setter =  accessorM mfieldSetA

--------------------------------------------------------------------------------

newtype instance MFieldC m e FieldModifyA = MFieldModifyA
  {
    mfieldModifyA :: Var m (MModifier m e)
  } deriving ( Typeable )

-- | Mutable field (pure) modifier.
type MModifier m e = (e -> e) -> m e

-- | 'GMField' mutable modifier (pure).
modifier :: (MonadVar m, FieldModifyA ~:= as) => GMField as MFieldC m record e
                                              -> Field m record (MModifier m e)
modifier =  accessorM mfieldModifyA

--------------------------------------------------------------------------------

newtype instance MFieldC m e FieldModifyMA = MFieldModifyMA
  {
    mfieldModifyMA :: Var m (MModifierM m e)
  } deriving ( Typeable )

-- | Mutable field (monadic) modifier.
type MModifierM m e = (e -> m e) -> m e

-- | 'GMField' mutable modifier (monadic).
modifierM :: (MonadVar m, FieldModifyMA ~:= as) => GMField as MFieldC m record e
                                                -> Field m record (MModifierM m e)
modifierM =  accessorM mfieldModifyMA

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
newtype GMField as mfieldC m record e = GMField
  {
    fromGMField :: record -> GMFieldRef as mfieldC m e
  } deriving ( Typeable )

-- | 'GMField' synonym with getter, setter and modifiers.
type MField = GMField FieldA MFieldC

instance (MonadVar m, FieldGetA ~:= as) => IsField
    (GMField as MFieldC m record e)
    (FieldC m record e) FieldGetA
  where
    fromField (GMField f) = FieldGetA $ join.get this.mfieldGetA <=< mfieldRef.f

instance (MonadVar m, FieldSetA ~:= as) => IsField
    (GMField as MFieldC m record e)
    (FieldC m record e) FieldSetA
  where
    fromField (GMField f) = FieldSetA . flip $ \ val ->
      ($ val) <=< get this.mfieldSetA <=< mfieldRef.f

instance (MonadVar m, FieldModifyA ~:= as) => IsField
    (GMField as MFieldC m record e)
    (FieldC m record e) FieldModifyA
  where
    fromField (GMField f) = FieldModifyA . flip $ \ g ->
      ($ g) <=< get this.mfieldModifyA <=< mfieldRef.f

instance (MonadVar m, FieldModifyMA ~:= as) => IsField
    (GMField as MFieldC m record e)
    (FieldC m record e) FieldModifyMA
  where
    fromField (GMField f) = FieldModifyMA . flip $ \ g ->
      ($ g) <=< get this.mfieldModifyMA <=< mfieldRef.f

--------------------------------------------------------------------------------

{- |
  @set recordX [fieldX =: fieldY $ recordY]@ makes @fieldX@ in @recordX@ refer
  to @fieldY@ in @recordY@ (override accessors).
-}
(=:) :: MonadVar m => GMField as MFieldC m record e -> record
                   -> GMField as MFieldC m record e -> Prop m record
field =: record = Property . LinkField field record

data LinkProp m record where
  LinkField :: MonadVar m => !(GMField as MFieldC m record e) -> record
                          -> !(GMField as MFieldC m record e)
                          -> LinkProp m record

instance IsProp LinkProp
  where
    performProp record1 (LinkField (GMField field1) record2 (GMField field2)) =
      setRecord this(fromGMFieldRef $ field1 record1) <=<
      getRecord this.fromGMFieldRef $ field2 record2

