{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE Trustworthy, MagicHash, CPP, UndecidableInstances #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators #-}

{- |
    Module      :  Data.Field.Type
    Copyright   :  (c) Andrey Mulik 2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "Data.Field.Type" - generalized field type.
-}
module Data.Field.Type
(
  -- * Field
  FieldT ( Field ), Attr, Attr',
  
  Attribute (..), field, field#,
  
  IsMVar (..), UniqueList, UniqueNames,
  
  UseAttribute (..), HasAttribute, updateAttribute,
  
  Prop ( Prop ), SomeProp ( SomeProp ), prop#, prop,
  
  AccessRep (..), AccessUse, IsField, UseField, use, use#, sets,
  
  get, set, modify, modifyM,
  
  get', set', modify', modifyM',
  
  getter, setter, modifier, modifierM,
  
  identityP, strefP, iorefP, mvarP, tvarP
)
where

import Data.Default.Class
import Data.Proxy
import Data.Kind

import Data.Field.Utils
import Data.Field.Prop

import Control.Applicative

import GHC.OverloadedLabels

default ()

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  @'FieldT' m api rep a@ is type of generalized field with some 'Prop's.
  
  * @m@ is 'Monad' in which 'AccessUse' actions are performed
  * @api@ is list of 'Attr' types which represents field's actions
  * @rep@ is the value type on which the methods are applied (representation)
  * @a@ is a type that represents a user-accessible value that can be obtained
  from or using given field for value @rep@.
  
  'FieldT' is mutable (or immutable) collection or stored attributes
  (or constants) and procedures (or functions). Unlike the OPP approach,
  @fmr@ separates data and methods - you can apply any field to any value, as
  long as the types allow it.
  
  @rep@ can be:
  
  * variable
  * collection
  * connection
  * typed stub
  * pool of connections
  * foreign API accessor
-}
data FieldT m api rep a
  where
    EmptyField :: FieldT m '[] rep a
    
    Field :: UniqueNames (Attr name subs : api)
          => FieldT m api rep a -> SomeProp name subs m rep a
          -> FieldT m (Attr name subs : api) rep a

instance Default (FieldT m '[] rep a) where def = EmptyField

--------------------------------------------------------------------------------

instance UseField "get" "" api => IsLabel "get" (FieldT m api rep a -> rep -> m a)
  where
#if MIN_VERSION_base(4,10,0)
    fromLabel = get
#else
    fromLabel _ = get
#endif

instance UseField "set" "" api => IsLabel "set" (FieldT m api rep a -> rep -> a -> m ())
  where
#if MIN_VERSION_base(4,10,0)
    fromLabel = set
#else
    fromLabel _ = set
#endif

instance UseField "modify" "" api => IsLabel "modify" (FieldT m api rep a -> rep -> (a -> a) -> m a)
  where
#if MIN_VERSION_base(4,10,0)
    fromLabel = modify
#else
    fromLabel _ = modify
#endif

instance UseField "modifyM" "" api => IsLabel "modifyM" (FieldT m api rep a -> rep -> (a -> m a) -> m a)
  where
#if MIN_VERSION_base(4,10,0)
    fromLabel = modifyM
#else
    fromLabel _ = modifyM
#endif

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Get some value.
-}
get :: UseField "get" "" api => FieldT m api rep a
    -> rep -> m a
get =  use# (proxy# :: Proxy# "get") (proxy# :: Proxy# "")

{- |
  @since 0.3
  
  Set some value.
-}
set :: UseField "set" "" api => FieldT m api rep a
    -> rep -> a -> m ()
set =  use# (proxy# :: Proxy# "set") (proxy# :: Proxy# "")

{- |
  @since 0.3
  
  Modify some value with function.
-}
modify :: UseField "modify" "" api => FieldT m api rep a
       -> rep -> (a -> a) -> m a
modify =  use# (proxy# :: Proxy# "modify") (proxy# :: Proxy# "")

{- |
  @since 0.3
  
  Modify some value with procedure.
-}
modifyM :: UseField "modifyM" "" api => FieldT m api rep a
        -> rep -> (a -> m a) -> m a
modifyM =  use# (proxy# :: Proxy# "modifyM") (proxy# :: Proxy# "")

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Get some accessor.
-}
get' :: UseField name "get" api => Proxy name -> FieldT m api rep a
     -> AccessUse name "get" m rep a
get' =  flip use (Proxy :: Proxy "get")

{- |
  @since 0.3
  
  Set some accessor.
-}
set' :: UseField name "set" api => Proxy name -> FieldT m api rep a
     -> AccessUse name "set" m rep a
set' =  flip use (Proxy :: Proxy "set")

{- |
  @since 0.3
  
  Modify some accessor with function.
-}
modify' :: UseField name "modify" api => Proxy name -> FieldT m api rep a
        -> AccessUse name "modify" m rep a
modify' =  flip use (Proxy :: Proxy "modify")

{- |
  @since 0.3
  
  Modify some accessor with procedure.
-}
modifyM' :: UseField name "modifyM" api => Proxy name -> FieldT m api rep a
         -> AccessUse name "modifyM" m rep a
modifyM' =  flip use (Proxy :: Proxy "modifyM")

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Get accessor from field.
-}
getter :: UseField name "get" api => FieldT m api rep a
       -> m (AccessRep name "" m rep a)
getter =  use Proxy (Proxy :: Proxy "get")

{- |
  @since 0.3
  
  Set accessor from field.
-}
setter :: UseField name "set" api => FieldT m api rep a
       -> AccessRep name "" m rep a -> m ()
setter =  use Proxy (Proxy :: Proxy "set")

{- |
  @since 0.3
  
  Modify accessor from field by function.
-}
modifier :: UseField name "modify" api => FieldT m api rep a
         -> (AccessRep name "" m rep a -> AccessRep name "" m rep a)
         -> m (AccessRep name "" m rep a)
modifier =  use Proxy (Proxy :: Proxy "modify")

{- |
  @since 0.3
  
  Modify accessor from field by prcedure.
-}
modifierM :: UseField name "modifyM" api => FieldT m api rep a
          -> (AccessRep name "" m rep a -> m (AccessRep name "" m rep a))
          -> m (AccessRep name "" m rep a)
modifierM =  use Proxy (Proxy :: Proxy "modifyM")

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'Proxy' synonym for 'Data.Field.Field' with 'Identity'-stored 'Prop's.
-}
identityP :: Proxy Identity
identityP =  Proxy

{- |
  @since 0.3
  
  'Proxy' synonym for 'Data.Field.Field' with 'STRef'-stored 'Prop's.
-}
strefP :: Proxy (STRef s)
strefP =  Proxy

{- |
  @since 0.3
  
  'Proxy' synonym for 'Data.Field.Field' with 'IORef'-stored 'Prop's.
-}
iorefP :: Proxy IORef
iorefP =  Proxy

{- |
  @since 0.3
  
  'Proxy' synonym for 'Data.Field.Field' with 'MVar'-stored 'Prop's.
-}
mvarP :: Proxy MVar
mvarP =  Proxy

{- |
  @since 0.3
  
  'Proxy' synonym for 'Data.Field.Field' with 'TVar'-stored 'Prop's.
-}
tvarP :: Proxy TVar
tvarP =  Proxy

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  @'UseField' name sub api@ checks if @api@ list contain @'Attr' name subs@
  type with given @name@ and @subs@ list with given @sub@.
-}
class UseField name sub api
  where
    useProp :: Proxy# name -> Proxy# sub -> FieldT m api rep a
            -> AccessUse name sub m rep a

instance {-# INCOHERENT #-} (HasAttribute sub subs, HasAttribute "" subs)
      => UseField name sub (Attr name subs : api)
  where
    useProp _ sub# (Field _ prop') = useAttribute sub# prop'

instance {-# INCOHERENT #-} (UseField name sub api, name /= name')
      => UseField name sub (Attr name' sub' : api)
  where
    useProp name# sub# (Field fld _) = useProp name# sub# fld

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  @'use#' name# sub# fld@ returns 'AccessUse' (action with given @name@ and
  @sub@) for given field.
-}
use# :: UseField name sub api => Proxy# name -> Proxy# sub
     -> FieldT m api rep a -> AccessUse name sub m rep a
use# =  useProp

{- |
  @since 0.3
  
  @'use' name sub fld@ returns 'AccessUse' (action with given @name@ and
  @sub@) for given field.
-}
use :: UseField name sub api => Proxy name -> Proxy sub
    -> FieldT m api rep a -> AccessUse name sub m rep a
use =  \ name sub fld -> useProp (toProxy# name) (toProxy# sub) fld

{- |
  @since 0.3
  
  Apply actions to given value and discard results.
-}
sets :: Monad m => rep -> [RunFieldT m rep] -> m ()
sets =  mapM_ . flip ($)

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  @'IsField' m var api rep a@ checks if @'FieldT' m api rep a@ can be created
  with accessors represented as @var ('AccessRep' name sub m rep a)@.
-}
class Monad new => IsField m new (var :: Type -> Type) api rep a
  where
    createField :: Proxy# var -> new (FieldT m api rep a)

instance Monad new => IsField m new var '[] rep a
  where
    createField _ = return def

instance
    (
      IsMVar m var,
      IsField m new var api rep a,
      IsProp m new var name subs rep a,
      UniqueNames (Attr name subs : api)
    ) => IsField m new var (Attr name subs : api) rep a
  where
    createField var# = liftA2 (flip Field . SomeProp) (prop# var#) (createField var#)

{- |
  @since 0.3
  
  Create field with accessors represented as @var ('AccessRep' name sub m rep a)@.
-}
field# :: IsField m new var api rep a => Proxy# var -> new (FieldT m api rep a)
field# =  createField

{- |
  @since 0.3
  
  Create field with accessors represented as @var ('AccessRep' name sub m rep a)@.
-}
field :: IsField m new var api rep a => Proxy var -> new (FieldT m api rep a)
field =  \ p -> createField (toProxy# p)
