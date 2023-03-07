{-# LANGUAGE Safe, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DataKinds, ConstraintKinds, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

{- |
    Module      :  Data.Field.Prop.Types
    Copyright   :  (c) Andrey Mulik 2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "Data.Field.Prop.Types" defines 'AccessRep' and 'AccessUse' type families.
-}
module Data.Field.Prop.Types
(
  -- * Export
  module Data.Field.Utils,
  
  -- * Attribute
  AccessRep (..), AccessUse,
  
  -- ** Attribute access
  Attr, Attr', UniqueNames, NamesOf,
  
  -- ** attribute usage
  UseAttribute (..), Attribute (..),
  
  -- * Misc
  Type, Symbol
)
where
  
import Data.Field.Utils
import Data.Kind

import GHC.TypeLits

default ()

--------------------------------------------------------------------------------

{- Attribute representation. -}

{- |
  @since 0.3
  
  Representation of attribute in 'Data.Field.FieldT'.
-}
data family AccessRep (name :: Symbol) (sub :: Symbol) (m :: Type -> Type) rep a

newtype instance AccessRep "get" "" m rep a = AccessGet
  {
    accessGet :: AccessUse "get" "" m rep a
  }

newtype instance AccessRep "set" "" m rep a = AccessSet
  {
    accessSet :: AccessUse "set" "" m rep a
  }

newtype instance AccessRep "modify" "" m rep a = AccessModify
  {
    accessModify :: AccessUse "modify" "" m rep a
  }

newtype instance AccessRep "modifyM" "" m rep a = AccessModifyM
  {
    accessModifyM :: AccessUse "modifyM" "" m rep a
  }

data instance AccessRep name "get" m rep a

data instance AccessRep name "set" m rep a

data instance AccessRep name "modify" m rep a

data instance AccessRep name "modifyM" m rep a

--------------------------------------------------------------------------------

{- Attribute usage. -}

{- |
  @since 0.3
  
  Type of action of attribute in 'Data.Field.FieldT'.
-}
type family AccessUse (name :: Symbol) (sub :: Symbol) (m :: Type -> Type) rep a

type instance AccessUse "get" "" m rep a = rep -> m a

type instance AccessUse "set" "" m rep a = rep -> a -> m ()

type instance AccessUse "modify" "" m rep a = rep -> (a -> a) -> m a

type instance AccessUse "modifyM" "" m rep a = rep -> (a -> m a) -> m a

type instance AccessUse name "get" m rep a = m (AccessRep name "" m rep a)

type instance AccessUse name "set" m rep a = AccessRep name "" m rep a -> m ()

type instance AccessUse name "modify" m rep a
  = (AccessRep name "" m rep a -> AccessRep name "" m rep a) ->
  m (AccessRep name "" m rep a)

type instance AccessUse name "modifyM" m rep a
  = (AccessRep name "" m rep a -> m (AccessRep name "" m rep a)) ->
  m (AccessRep name "" m rep a)

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'Attr' represents 'Prop' or 'SomeProp' attributes.
  For most use cases, you should use 'Attr''.
-}
data Attr (name :: Symbol) (subs :: [Symbol])

{- |
  @since 0.3
  
  'Attr'' is 'Attr' with implicit @""@ attribute, which used for basic action.
  
  Example:
  
  @
  'Attr'' "get"    '[]         -- accessible getter.
  'Attr'' "get"    '["get"]    -- accessible and readable getter.
  'Attr'' "set"    '["set"]    -- accessible and replaceable setter.
  'Attr'' "modify" '["modify"] -- accessible and modifiable modifier.
  
  -- Accessible, readable, replaceable and modifiable modifier.
  'Data.Field.UpdateA' === 'Attr' "modifyM" '["", "get", "set", "modify", "modifyM"]
  @
-}
type Attr' name subs = Attr name ("" : subs)

{- |
  @since 0.3
  
  List of 'Attr' types whth unique names.
  
  Correct examples:
  
  @
  'UniqueNames' '[]
  'UniqueNames' '['Data.Field.GetA']
  'UniqueNames' ['Data.Field.GetA', 'Data.Field.SetA']
  'UniqueNames' ['Data.Field.GetA', 'Data.Field.SetA', 'Data.Field.ModifyA']
  @
  
  Wrong examples:
  
  @
  -- explicit duplicate
  'UniqueNames' ['Data.Field.GetA', 'Data.Field.GetA']
  
  -- also duplicate, because 'Data.Field.GetA' and 'Data.Field.ReadA' is both ('Attr' "get")-based.
  'UniqueNames' ['Data.Field.GetA', 'Data.Field.ReadA']
  @
-}
type UniqueNames api = UniqueList (NamesOf api)

type family NamesOf (api :: [Type])
  where
    NamesOf                    '[] = '[]
    NamesOf (Attr name subs : api) = name : NamesOf api
    NamesOf            (typ : api) = TypeError
      (
        'Text "Expected 'Attr' type, actual: " ':<>: 'ShowType typ ':<>:
        'Text " in type list " ':<>: 'ShowType (typ : api)
      )

--------------------------------------------------------------------------------

type GetAttribute var name sub rep m a = Attribute sub "" m
                    (var (AccessRep name "" m rep a))
                         (AccessRep name "" m rep a)

--------------------------------------------------------------------------------

{- Use attribute. -}

{- |
  @since 0.3
  
  Class of attributes which can be used. Required for all attributes in 'Prop'
  (and 'SomeProp', and 'Data.Field.Type.FieldT').
-}
class IsMVar m var => UseAttribute var name sub m rep a
  where
    useAttr :: var (AccessRep name sub m rep a)
            -> var (AccessRep name ""  m rep a)
            -> AccessUse name sub m rep a

instance IsMVar m var => UseAttribute var "get" "" m rep a
  where
    useAttr = const $ \ get' rep -> do
      g <- fromMRef get'
      accessGet g rep

instance IsMVar m var => UseAttribute var "set" "" m rep a
  where
    useAttr = const $ \ set' rep a -> do
      s <- fromMRef set'
      accessSet s rep a

instance IsMVar m var => UseAttribute var "modify" "" m rep a
  where
    useAttr = const $ \ modify' rep f -> do
      m <- fromMRef modify'
      accessModify m rep f

instance IsMVar m var => UseAttribute var "modifyM" "" m rep a
  where
    useAttr = const $ \ modifyM' rep go -> do
      m <- fromMRef modifyM'
      accessModifyM m rep go

instance (IsMVar m var, GetAttribute var name "get" rep m a)
      => UseAttribute var name "get" m rep a
  where
    useAttr = const $ accessGet attribute

instance (IsMVar m var, GetAttribute var name "set" rep m a)
      => UseAttribute var name "set" m rep a
  where
    useAttr = const $ accessSet attribute

instance (IsMVar m var, GetAttribute var name "modify" rep m a)
      => UseAttribute var name "modify" m rep a
  where
    useAttr = const $ accessModify attribute

instance (IsMVar m var, GetAttribute var name "modifyM" rep m a)
      => UseAttribute var name "modifyM" m rep a
  where
    useAttr = const $ accessModifyM attribute

--------------------------------------------------------------------------------

{- Make attribute. -}

{- |
  @since 0.3
  
  Class of default/initial values of named attribute.
-}
class Attribute name sub m rep a
  where
    attribute :: AccessRep name sub m rep a

instance Attribute "get" "" Identity a a where attribute = AccessGet return

instance Attribute "get" "" Identity (Identity a) a where attribute = AccessGet id

instance Attribute "get"     "" (ST s) (STRef s a) a where attribute = AccessGet readSTRef
instance Attribute "set"     "" (ST s) (STRef s a) a where attribute = AccessSet writeSTRef
instance Attribute "modify"  "" (ST s) (STRef s a) a where attribute = AccessModify updateSTRef
instance Attribute "modifyM" "" (ST s) (STRef s a) a where attribute = AccessModifyM updateMSTRef

instance Attribute "get"     "" IO (IORef a) a where attribute = AccessGet readIORef
instance Attribute "set"     "" IO (IORef a) a where attribute = AccessSet writeIORef
instance Attribute "modify"  "" IO (IORef a) a where attribute = AccessModify updateIORef
instance Attribute "modifyM" "" IO (IORef a) a where attribute = AccessModifyM updateMIORef

instance Attribute "get"     "" IO (MVar a) a where attribute = AccessGet readMVar
instance Attribute "set"     "" IO (MVar a) a where attribute = AccessSet writeMVar
instance Attribute "modify"  "" IO (MVar a) a where attribute = AccessModify updateMVar
instance Attribute "modifyM" "" IO (MVar a) a where attribute = AccessModifyM updateMMVar

instance Attribute "get"     "" STM (TVar a) a where attribute = AccessGet readTVar
instance Attribute "set"     "" STM (TVar a) a where attribute = AccessSet writeTVar
instance Attribute "modify"  "" STM (TVar a) a where attribute = AccessModify updateTVar
instance Attribute "modifyM" "" STM (TVar a) a where attribute = AccessModifyM updateMTVar

instance Monad m => Attribute name "get"     m rep a where attribute = undefined
instance Monad m => Attribute name "set"     m rep a where attribute = undefined
instance Monad m => Attribute name "modify"  m rep a where attribute = undefined
instance Monad m => Attribute name "modifyM" m rep a where attribute = undefined




