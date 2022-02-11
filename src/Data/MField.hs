{-# LANGUAGE GADTs, TypeOperators, DataKinds, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE Safe, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}

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
  IsRef (..), GMFieldRef (..), MFieldRef, MFieldRep
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
newtype GMFieldRef m e as = GMFieldRef {fromGMFieldRef :: Var m (FObject (MFieldC m e) as)}

{- |
  'MFieldRef' is a structure containing accessors for a specific field of a
  specific record. As a rule, it makes sense to store 'MFieldRef' in the record
  to which it refers.
-}
type MFieldRef m e = GMFieldRef m e [FieldGetA, FieldSetA, FieldModifyA, FieldModifyMA]

-- | Get 'GMFieldRef' getter.
getRef :: (MonadVar m, FieldGetA ~:= as) => GMFieldRef m e as -> m (MGetter m e)
getRef =  fmap (mFieldGetA.fromField).get this.fromGMFieldRef

-- | Set 'GMFieldRef' getter.
setRef :: (MonadVar m, FieldSetA ~:= as) => GMFieldRef m e as -> m (MSetter m e)
setRef =  fmap (mFieldSetA.fromField).get this.fromGMFieldRef

-- | Set 'GMFieldRef' (pure) modifier.
modifyRef :: (MonadVar m, FieldModifyA ~:= as) => GMFieldRef m e as -> m (MModifier m e)
modifyRef =  fmap (mFieldModifyA.fromField).get this.fromGMFieldRef

-- | Set 'GMFieldRef' (monadic) modifier.
modifyMRef :: (MonadVar m, FieldModifyMA ~:= as) => GMFieldRef m e as -> m (MModifierM m e)
modifyMRef =  fmap (mFieldModifyMA.fromField).get this.fromGMFieldRef

--------------------------------------------------------------------------------

-- | 'MFieldRep' is a helper type that stores a variable and its accessors.
data GMFieldRep m e as = GMFieldRep !(Var m e) !(GMFieldRef m e as)
  deriving ( Typeable )

-- | 'GMFieldRep' synonym with getter, setter and modifiers.
type MFieldRep m e = GMFieldRep m e [FieldGetA, FieldSetA, FieldModifyA, FieldModifyMA]

--------------------------------------------------------------------------------

-- | Class of variable field accessors.
class IsRef ref m e | ref -> m, ref -> e
  where
    -- | Create new reference to the given variable.
    link :: MonadVar m => Var m e -> m ref
    
    -- | Get an associated 'MFieldRef' reference to the given reference.
    ref :: MonadVar m => ref -> MFieldRef m e

instance IsRef (MFieldRef m e) m e
  where
    ref    = id
    link x = let Field g s m mm = this in do
      g' <- MFieldGetA    <$> var (g x); s'  <- MFieldSetA     <$> var (s  x)
      m' <- MFieldModifyA <$> var (m x); mm' <- MFieldModifyMA <$> var (mm x)
      GMFieldRef <$> var (g' :++ s' :++ m' :++ mm' :++ FObjectEmpty)

instance IsRef (MFieldRep m e) m e
  where
    link x = GMFieldRep x <$> link x
    ref (GMFieldRep _ fr) = fr

--------------------------------------------------------------------------------

-- | Mutable field convention.
data family MFieldC (m :: Type -> Type) e (a :: Symbol) :: Type

newtype instance MFieldC m e FieldGetA = MFieldGetA
  {mFieldGetA :: MGetter m e}

-- | Mutable field getter.
type MGetter m e = Var m (m e)

newtype instance MFieldC m e FieldSetA = MFieldSetA
  {mFieldSetA :: MSetter m e}

-- | Mutable field setter.
type MSetter m e = Var m (e -> m ())

newtype instance MFieldC m e FieldModifyA = MFieldModifyA
  {mFieldModifyA :: MModifier m e}

-- | Mutable field (pure) modifier.
type MModifier m e = Var m ((e -> e) -> m e)

-- | Mutable field (monadic) modifier.
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

-- | 'GMField' synonym with getter, setter and modifiers.
type MField m record e = GMField m record e [FieldGetA, FieldSetA, FieldModifyA, FieldModifyMA]

instance MonadVar m => IsField (GMField m record e (as :: [Symbol])) (FieldC m record e) FieldGetA
  where
    type FieldGetA ~?= GMField m record e as = FieldGetA ~:= as
    
    fromField (GMField f) = FieldGetA $ join.get this <=< getRef.f

instance MonadVar m => IsField (GMField m record e as) (FieldC m record e) FieldSetA
  where
    type FieldSetA ~?= GMField m record e as = FieldSetA ~:= as
    
    fromField (GMField f) = FieldSetA . flip $ \ val ->
      ($ val) <=< get this <=< setRef.f

instance MonadVar m => IsField (GMField m record e as) (FieldC m record e) FieldModifyA
  where
    type FieldModifyA ~?= GMField m record e as = FieldModifyA ~:= as
    
    fromField (GMField f) = FieldModifyA . flip $ \ g ->
      ($ g) <=< get this <=< modifyRef.f

instance MonadVar m => IsField (GMField m record e as) (FieldC m record e) FieldModifyMA
  where
    type FieldModifyMA ~?= GMField m record e as = FieldModifyMA ~:= as
    
    fromField (GMField f) = FieldModifyMA . flip $ \ g ->
      ($ g) <=< get this <=< modifyMRef.f

--------------------------------------------------------------------------------

-- | 'GMField' mutable getter.
getter :: (MonadVar m, FieldGetA ~:= as) => GMField m record e as
       -> Field m record (m e)
getter (GMField f) = Field  (getRecord     this <=< getRef.f)
      (flip $ \ x -> ($ x) . setRecord     this <=< getRef.f)
      (flip $ \ x -> ($ x) . modifyRecord  this <=< getRef.f)
      (flip $ \ x -> ($ x) . modifyRecordM this <=< getRef.f)

-- | 'GMField' mutable setter.
setter :: (MonadVar m, FieldSetA ~:= as) => GMField m record e as
       -> Field m record (e -> m ())
setter (GMField f) = Field  (getRecord     this <=< setRef.f)
      (flip $ \ x -> ($ x) . setRecord     this <=< setRef.f)
      (flip $ \ x -> ($ x) . modifyRecord  this <=< setRef.f)
      (flip $ \ x -> ($ x) . modifyRecordM this <=< setRef.f)

-- | 'GMField' mutable modifier (pure).
modifier :: (MonadVar m, FieldModifyA ~:= as) => GMField m record e as
         -> Field m record ((e -> e) -> m e)
modifier (GMField f) = Field  (getRecord     this <=< modifyRef.f)
        (flip $ \ x -> ($ x) . setRecord     this <=< modifyRef.f)
        (flip $ \ x -> ($ x) . modifyRecord  this <=< modifyRef.f)
        (flip $ \ x -> ($ x) . modifyRecordM this <=< modifyRef.f)

-- | 'GMField' mutable modifier (monadic).
modifierM :: (MonadVar m, FieldModifyMA ~:= as) => GMField m record e as
          -> Field m record ((e -> m e) -> m e)
modifierM (GMField f) = Field (getRecord     this <=< modifyMRef.f)
        (flip $ \ x -> ($ x) . setRecord     this <=< modifyMRef.f)
        (flip $ \ x -> ($ x) . modifyRecord  this <=< modifyMRef.f)
        (flip $ \ x -> ($ x) . modifyRecordM this <=< modifyMRef.f)

--------------------------------------------------------------------------------

(=:) :: (Typeable m, Typeable record, Typeable e, Typeable as, MonadVar m)
     => GMField m record e as -> record -> GMField m record e as
     -> Prop m (GMField m record e as) record
field =: record = Prop . LinkField field record

data LinkProp m field record
  where
    LinkField :: MonadVar m
      => !(GMField m record e as) -> record -> !(GMField m record e as)
      -> LinkProp m (GMField m record e as) record

instance IsProp LinkProp
  where
    performProp record1 (LinkField (GMField field1) record2 (GMField field2)) =
      setRecord this (fromGMFieldRef $ field1 record1) <=<
      getRecord this . fromGMFieldRef $ field2 record2


