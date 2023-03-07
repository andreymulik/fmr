{-# LANGUAGE Safe, MagicHash, DataKinds, GADTs, TypeFamilies, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

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
  
  Prop ( Prop ), SomeProp ( SomeProp ), prop#, prop,
  
  UseAttribute (..), HasAttribute, updateAttribute,
  
  Attribute (..), field, field#,
  
  AccessRep (..), AccessUse, IsField, UseField, use, use#,
  
  IsMVar (..), UniqueList, UniqueNames
)
where

import Data.Default.Class
import Data.Proxy
import Data.Kind

import Data.Field.Utils
import Data.Field.Prop

import Control.Applicative

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



