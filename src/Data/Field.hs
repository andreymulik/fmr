{-# LANGUAGE UndecidableSuperClasses, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE Trustworthy, TypeFamilies, FlexibleContexts, PatternSynonyms #-}

{- |
    License     :  BSD-style
    Module      :  Data.Field
    Copyright   :  (c) Andrey Mulik 2020
    Maintainer  :  work.a.mulik@gmail.com
    
    @Data.Field@ provides fake field type for record-style operations.
-}
module Data.Field
(
  -- * Field
  Field (.., SField, Field, getField, setField, modifyField),
  GetterFor, SetterFor, ModifierFor, ModifierMFor,
  
  -- * IsMVar and MonadVar
  IsMVar (..), MonadVar (..),
  
  -- ** Observable field
  Observe (..), OField, observe
)
where

import Prelude hiding ( (.), id )
import Data.Property
import Data.Typeable
import Data.Functor
import Data.IORef
import Data.STRef
import Data.Kind

import GHC.Conc

import Control.Concurrent.MVar
import Control.Category
import Control.Monad.ST
import Control.Monad

default ()

--------------------------------------------------------------------------------

{- |
  Normal field, which contain getter, setter and modifier.
  
  Since @fmr-0.2@, you can also combine fmr fields using @('.')@ and @'id'@ from
  the 'Category' class:
  
  @
    outer :: (Monad m) => Field m outer inner
    inner :: (Monad m) => Field m inner value
    
    field :: (Monad m) => Field m outer value
    field =  outer.inner
  @
-}
data Field m record a = Field'
    -- | Field getter
    !(GetterFor    m record a)
    -- | Field setter
    !(SetterFor    m record a)
    -- | Field modifier
    !(ModifierFor  m record a)
    -- | Monadic field modifier
    !(ModifierMFor m record a)
  deriving ( Typeable )

-- | Getter type.
type GetterFor    m record a = record -> m a

-- | Setter type.
type SetterFor    m record a = record -> a -> m ()

-- | Modifier type.
type ModifierFor  m record a = record -> (a -> a) -> m a

-- | Monadic modifier type.
type ModifierMFor m record a = record -> (a -> m a) -> m a

{- |
  'SField' pattern simplifies field creation if modifiers uses getter and setter.
-}
pattern SField :: (Monad m) => GetterFor m record a -> SetterFor m record a -> Field m record a
pattern SField g s <- Field' g s _ _ where SField g s = Field' g s (modifyDummy g s) (modifyMDummy g s)

modifyDummy :: (Monad m) => GetterFor m record a -> SetterFor m record a -> ModifierFor m record a
modifyDummy g s = \ record f -> do val <- f <$> g record; s record val; return val

{- |
  'Field' pattern simplifies field creation if monadic modifier uses getter and
  setter.
-}
pattern Field :: (Monad m) => GetterFor m record a -> SetterFor m record a -> ModifierFor m record a -> Field m record a
pattern Field{getField, setField, modifyField} <- Field' getField setField modifyField _
  where
    Field g s m = Field' g s m (modifyMDummy g s)

modifyMDummy :: (Monad m) => GetterFor m record a -> SetterFor m record a -> ModifierMFor m record a
modifyMDummy g s = \ record f -> do val <- f =<< g record; s record val; return val

--------------------------------------------------------------------------------

instance (Monad m) => Category (Field m)
  where
    Field' g1 s1 m1 mm1 . Field' g2 _ _ _ = Field' (g1 <=< g2) s3 m3 mm3
      where
        mm3 record   go  = flip mm1  go  =<< g2 record
        m3  record   f   = flip m1   f   =<< g2 record
        s3  record value = flip s1 value =<< g2 record
    
    id = Field' return (\ _ _ -> return ()) (return ... flip ($)) (flip ($))

--------------------------------------------------------------------------------

instance FieldGet    Field where getRecord    = getField
instance FieldSet    Field where setRecord    = setField
instance FieldModify Field where modifyRecord = modifyField

instance FieldSwitch Field
  where
    switchRecord field record = void . modifyRecord field record . toggle

--------------------------------------------------------------------------------

-- | Simple field observer, which can run some handlers after each action.
data Observe field m record a = Observe
  {
    -- | Field to observe.
    observed :: field m record a,
    -- | 'getRecord' observer
    onGet    :: record -> a -> m (),
    -- | 'setRecord' observer
    onSet    :: record -> a -> m (),
    -- | 'modifyRecord' observer
    onModify :: record -> m ()
  }
  deriving ( Typeable )

-- | Observable 'Field'.
type OField = Observe Field

-- | Create field with default observers.
observe :: (Monad m) => field m record a -> Observe field m record a
observe field =
  let og = \ _ _ -> return (); om = \ _ -> return (); os = og
  in  Observe field og os om

--------------------------------------------------------------------------------

instance (FieldSwitch field) => FieldSwitch (Observe field)
  where
    switchRecord field record n = do
      switchRecord (observed field) record n
      onModify field record

instance (FieldGet field) => FieldGet (Observe field)
  where
    getRecord field record = do
      res <- getRecord (observed field) record
      onGet field record res
      return res

instance (FieldSet field) => FieldSet (Observe field)
  where
    setRecord field record val = do
      setRecord (observed field) record val
      onSet field record val

instance (FieldModify field, FieldGet field) => FieldModify (Observe field)
  where
    modifyRecord field record upd = do
      res <- modifyRecord (observed field) record upd
      onModify field record
      return res
    
    modifyRecordM field record upd = do
      res <- modifyRecordM (observed field) record upd
      onModify field record
      return res

--------------------------------------------------------------------------------

{- |
  'MonadVar' is a class of monads for which defined at least one type of mutable
  variable.
  
  Note that a variable of type @(Var m)@ should be as simple possible for a
  given monad. It doesn't have high requirements (e.g. thread safety), it only
  has to implement the basic triad of operations: read, write and update (which
  don't have to be atomic).
-}
class (Monad m, IsMVar m (Var m)) => MonadVar m
  where
    -- | @('Var' m)@ is type of mutable variable in monad @m@.
    type Var m :: Type -> Type

instance MonadVar (ST s) where type Var (ST s) = STRef s
instance MonadVar IO     where type Var IO     = IORef
instance MonadVar STM    where type Var STM    = TVar

--------------------------------------------------------------------------------

{- |
  The 'IsMVar' class provides 'this' field for entire record access.
  
  It's supposed to be useful for working with mutable variables and structures
  when @(':~')@ is difficult or impairs the readability of the code, e.g:
  
  @
    set record [this := value] === set record [anyField ::= const value]
    set record [this := value] === set record [anyField ::~ const value]
  @
  
  Please note that you cannot create 'IsMVar' and 'MonadVar' instances for some
  monad separately.
-}
class (Monad m, MonadVar m) => IsMVar m var
  where
    -- | 'this' is common variable access field.
    this :: Field m (var a) a
    
    -- | Create and initialize new mutable variable.
    var :: a -> m (var a)

instance IsMVar (ST s) (STRef s)
  where
    var  = newSTRef
    this = Field' readSTRef writeSTRef modify' modifyM'
      where
        modifyM' ref f = do res <- f =<< readSTRef ref; res <$ writeSTRef ref res
        modify'  ref f = do res <- f <$> readSTRef ref; res <$ writeSTRef ref res

instance IsMVar IO IORef
  where
    var  = newIORef
    this = -- monadic modifier is dummy for now
      let modify' ref f = ref `atomicModifyIORef'` \ a -> let b = f a in (b, b)
      in  Field readIORef writeIORef modify'

instance IsMVar IO MVar
  where
    var  = newMVar
    this = Field' readMVar putMVar modify' modifyM'
      where
        modifyM' mvar f = mvar `modifyMVarMasked` \ a -> do b <- f a; return (b, b)
        modify'  mvar f = mvar `modifyMVar` \ a -> let b = f a in return (b, b)

instance IsMVar STM TVar
  where
    var  = newTVar
    this = Field' readTVar writeTVar modifyTVar modifyMTVar
      where
        modifyMTVar tvar f = do res <- f =<< readTVar tvar; res <$ writeTVar tvar res
        modifyTVar  tvar f = do res <- f <$> readTVar tvar; res <$ writeTVar tvar res

--------------------------------------------------------------------------------

-- | @sdp@ @(.)@-like combinator.
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) =  (.) . (.)



