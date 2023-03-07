{-# LANGUAGE MultiParamTypeClasses, UndecidableSuperClasses, UndecidableInstances #-}
{-# LANGUAGE Safe, MagicHash, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators #-}

{- |
    Module      :  Data.Field.Prop
    Copyright   :  (c) Andrey Mulik 2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "Data.Field.Prop" property representation.
-}
module Data.Field.Prop
(
  -- * Property type
  Prop ( Prop, getProp, getAttr ), SomeProp ( SomeProp ),
  
  -- ** Prop representation
  module Data.Field.Prop.Types,
  
  -- ** Check if 'Prop' contain attribute
  HasAttribute, useAttribute, updateAttribute,
  
  -- ** Create property
  IsProp, prop, prop#
)
where

import Data.Field.Prop.Types
import Data.Default.Class

import Control.Applicative

default ()

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'SomeProp' is @var@-agnostic version of 'Prop' with restricted list of @subs@.
  
  The attributes of 'SomeProp' can be used without further knowledge of the
  internal structure of 'Prop'. 
-}
data SomeProp name subs m rep a
  where
    SomeProp :: UniqueList subs
             => Prop var name subs m rep a
             -> SomeProp name subs m rep a

instance Default (SomeProp name '[] m rep a) where def = SomeProp def

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'Prop' is typed list of 'AccessRep' (representation of method).
-}
data Prop var name subs m rep a
  where
    EmProp :: Prop var name '[] m rep a
    
    Prop :: UseAttribute var name sub m rep a =>
      {
        getProp :: Prop var name subs m rep a,
        getAttr :: var (AccessRep name sub m rep a)
      } -> Prop var name (sub : subs) m rep a

instance Default (Prop var name '[] m rep a) where def = EmProp

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  @'IsProp' m var name subs rep a@ checks if @'Prop' var name subs m rep a@
  can be created with accessors represented as @var ('AccessRep' name sub m rep a)@.
-}
class UniqueList subs => IsProp m new (var :: Type -> Type) name subs rep a
  where
    createProp :: Proxy# var -> new (Prop var name subs m rep a)

instance Monad new => IsProp m new var name '[] rep a
  where
    createProp _ = return def

instance
  (
    IsMVar new var,
    UniqueList (sub : subs),
    Attribute name sub m rep a,
    UseAttribute var name sub m rep a,
    IsProp m new var name subs rep a
  ) => IsProp m new var name (sub : subs) rep a
  where
    createProp var# = liftA2 Prop (createProp var#) (var attribute)

{- |
  @since 0.3
  
  Create 'Prop' with accessors represented as @var ('AccessRep' name sub m rep a)@.
-}
prop# :: IsProp m new var name subs rep a => Proxy# var
      -> new (Prop var name subs m rep a)
prop# =  createProp

{- |
  @since 0.3
  
  Create 'Prop' with accessors represented as @var ('AccessRep' name sub m rep a)@.
-}
prop :: IsProp m new var name subs rep a => Proxy var
     -> new (Prop var name subs m rep a)
prop =  \ p# -> createProp (toProxy# p#)

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  @'HasAttribute' sub subs@ checks if attribute list @subs@ contain @sub@.
-}
class HasAttribute sub subs
  where
    hasAttr :: Prop var name subs m rep a -> Prop var name '[sub] m rep a
    
    updateAttr :: Prop var name subs m rep a -> Prop var name '[sub] m rep a
               -> Prop var name subs m rep a

instance {-# INCOHERENT #-} HasAttribute sub (sub : subs)
  where
    hasAttr    (Prop _ x) = Prop def x
    updateAttr (Prop p _) = Prop p . getAttr

instance {-# INCOHERENT #-} (sub /= sub', HasAttribute sub subs)
      => HasAttribute sub (sub' : subs)
  where
    hasAttr               = hasAttr . getProp
    updateAttr (Prop p x) = (`Prop` x) . updateAttr p

--------------------------------------------------------------------------------

useAttribute :: (HasAttribute sub subs, HasAttribute "" subs) => Proxy# sub
             -> SomeProp name subs m rep a -> AccessUse name sub m rep a

useAttribute sub# (SomeProp prop') = case hasAttr prop' of
    Prop _ sub -> useAttr# sub# sub rep
  where
    Prop _ rep = hasAttr prop'

useAttr# :: UseAttribute var name sub m rep a => Proxy# sub
         -> var (AccessRep name sub m rep a)
         -> var (AccessRep name ""  m rep a)
         -> AccessUse name sub m rep a
useAttr# =  \ _ -> useAttr

{- |
  @since 0.3
  
  Replace attribute.
-}
updateAttribute :: HasAttribute sub subs
                => Prop var name subs m rep a -> Prop var name '[sub] m rep a
                -> Prop var name subs m rep a
updateAttribute =  updateAttr


