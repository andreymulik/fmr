{-# LANGUAGE Safe, GADTs #-}

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
  MField (..), IsRef (..), MFieldRef, MFieldRep,
  getter, setter, modifier, modifierM
)
where

import Data.Typeable
import Data.Property
import Data.Field

import Control.Monad

default ()

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
data MField m record a
  where
    MField :: (MonadVar m) => (record -> MFieldRef m a) -> MField m record a
  deriving ( Typeable )

instance FieldGet MField
  where
    getRecord (MField field) = join.get this.getRef.field

instance FieldSet MField
  where
    setRecord (MField field) record value = do
      g <- get this.setRef.field $ record; g value

instance FieldModify MField
  where
    modifyRecord (MField field) record upd = do
      m <- get this.modifyRef.field $ record; m upd
    
    modifyRecordM (MField field) record go = do
      mm <- get this.modifyMRef.field $ record; mm go

instance FieldSwitch MField
  where
    switchRecord field record = void.modifyRecord field record.toggle

--------------------------------------------------------------------------------

class IsRef ref
  where
    -- | Create new reference to the given variable.
    link :: (MonadVar m) => Var m a -> m (ref m a)
    
    -- | Get an associated 'MFieldRef' reference to the given reference.
    ref :: (MonadVar m) => ref m a -> MFieldRef m a

--------------------------------------------------------------------------------

{- |
  'MFieldRef' is a structure containing accessors for a specific field of a
  specific record. As a rule, it makes sense to store 'MFieldRef' in the record
  to which it refers.
-}
data MFieldRef m a = MFieldRef
  {
    getRef     :: !(Var m (m a)),
    setRef     :: !(Var m (a -> m ())),
    modifyRef  :: !(Var m ((a -> a) -> m a)),
    modifyMRef :: !(Var m ((a -> m a) -> m a))
  } deriving ( Typeable )

instance IsRef MFieldRef
  where
    ref    = id
    link x = liftM4 MFieldRef (var (g x)) (var (s x)) (var (m x)) (var (mm x))
      where
        Field g s m mm = this

getter :: MField m record a -> Field m record (m a)
getter (MField f) =
  let Field g s m mm = this
  in  Field (g.getRef.f) (s.getRef.f) (m.getRef.f) (mm.getRef.f)

setter :: MField m record a -> Field m record (a -> m ())
setter (MField f) =
  let Field g s m mm = this
  in  Field (g.setRef.f) (s.setRef.f) (m.setRef.f) (mm.setRef.f)

modifier :: MField m record a -> Field m record ((a -> a) -> m a)
modifier (MField f) =
  let Field g s m mm = this
  in  Field (g.modifyRef.f) (s.modifyRef.f) (m.modifyRef.f) (mm.modifyRef.f)

modifierM :: MField m record a -> Field m record ((a -> m a) -> m a)
modifierM (MField f) =
  let Field g s m mm = this
  in  Field (g.modifyMRef.f) (s.modifyMRef.f) (m.modifyMRef.f) (mm.modifyMRef.f)

--------------------------------------------------------------------------------

-- | 'MFieldRep' is a helper type that stores a variable and its accessors.
data MFieldRep m a = MFieldRep !(Var m a) {-# UNPACK #-} !(MFieldRef m a)
  deriving ( Typeable )

instance IsRef MFieldRep
  where
    link x = MFieldRep x <$> link x
    ref (MFieldRep _ fr) = fr

