{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE Trustworthy, CPP, MagicHash, DataKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

{- |
    Module      :  Data.Field
    Copyright   :  (c) Andrey Mulik 2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "Data.Field" provides generic 'Field' type.
-}
module Data.Field
(
  -- * Quick start
  -- $quickStart
  
  -- * Field
  FieldT ( Field ),
  
  -- * Property
  -- ** Types
  Prop ( Prop ), SomeProp ( SomeProp ),
  
  -- ** Property variables
  IsMVar (..),
  
  -- * Field creation
  Attribute (..), AccessRep (..), IsField, field,
  
  -- * Field usage
  UseField, AccessUse,
  UseAttribute (..), var', use, sets,
  HasAttribute, updateAttribute,
  
  -- ** Basic accessors
  get, set, modify, modifyM,
  
  -- *** Mutable accessors
  get', set', modify', modifyM',
  
  -- ** Old-style patterns (one-way)
  pattern (:=), pattern (::=), pattern (:~), pattern (::~),
  
  -- ** Lifted 'Maybe' methods
  (?=), (?~), (??=), (??~),
  
  -- ** Multiple-field methods
  ($:=), ($:~), ($::=), ($::~),
  
  -- ** Lifted 'Maybe' multiple-field methods
  (?:=), (?:~), (?::=), (?::~),
  
  -- * Accessors
  Attr, Attr', GetA', SetA', ModifyA', ModifyMA', GetA, SetA, ModifyA, ModifyMA,
  ReadA, WriteA, UpdateA, UpdateMA,
  
  -- * Field types
  -- ** Identity
  Field, Identity, identityP,
  
  -- ** ST
  STField, ST, strefP, STRef, newSTRef,
  
  readSTRef, writeSTRef, updateSTRef, updateMSTRef,
  
  -- ** IO
  IOField, IO, iorefP, IORef, newIORef, mvarP, MVar, newMVar,
  
  readIORef, writeIORef, updateIORef, updateMIORef,
  readMVar, writeMVar, updateMVar, updateMMVar,
  
  -- ** STM
  TField, STM, tvarP, TVar, newTVar,
  
  readTVar, writeTVar, updateTVar, updateMTVar,
  
  -- * Misc
  UniqueList, UniqueNames, Proxy#, proxy#
)
where

import Prelude hiding ( read )

import Data.Field.Utils
import Data.Field.Type
import Data.Functor
import Data.Maybe

import GHC.OverloadedLabels

import Control.Monad

default ()

infixl 0 `Field`

--------------------------------------------------------------------------------

{- $quickStart
  __Example 1 (immutable 'Field'):__
  
  @
    runIdentity $ do
      idField <- 'field' 'identityP' :: 'Identity' ('FieldT' 'Identity' \'['GetA'] 'Int' 'Int')
      'get' idField 1 -- 'return' 1
  @
  
  Now we have the simplest field @idField@, let's look at its type:
  
  * generic field type 'FieldT' - the main type of the @fmr@ library we'll be
  work with;
  * monad 'Identity' means that all actions with the field will take place in
  the 'Identity' monad;
  * api @'['GetA']@ - a list of all available attributes;
  * rep (first 'Int') - value to be manipulated;
  * a (the second 'Int') is the value that we get as a result of the operation.
  
  The @idField@ field is immutable because stores all attributes in the
  \"container\" 'Identity' - this is determined by the 'field'\'s argument
  'identityP'.
  
  The value to which @idField@ is applied is also immutable, because all actions
  with it are performed in the 'Identity' monad - this is determined by the
  first argument 'FieldT' ('Identity').
  
  On the next line we apply the 'GetA' attribute of the @idField@ field to
  @rep@ and 'get' a value of type @Identity a@.
  
  'get' is a synonym for 'use' with a predefined attribute name. To use it, we
  need the "get" attribute in @idField@ which specified in the API as 'GetA'.
  
  __Example 2 (immutable 'IOField'):__
  
  Let's now create a more complex field that will interact with the mutable
  variable. For convenience, let's take the monad 'IO' and the variable 'IORef',
  and instead of @('FieldT' 'IO')@ we will write 'IOField'.
  
  @
    -- Create 'IORef' - mutable variable.
    ioref <- 'newIORef' (1 :: Int)
    
    -- Create immutable 'IOField' using 'field' procedure.
    let
        myField :: IOField '[GetA, SetA, ModifyA] (IORef Int) Int
        myField =  'runIdentity' $ 'field' 'identityP'
    
    'get' myField ioref   -- 'return' 1
    'set' myField ioref 2 -- 'return' ()
    'get' myField ioref   -- 'return' 2
    
    'modify' myField ioref 'pred' -- 'return' 1
    'get' myField ioref         -- 'return' 1
  @
  
  As you can see, our new field has different:
  
  * monad - 'IO' instead of 'Identity';
  * type @rep@ - now we work with a variable;
  * number of attributes in the API - now we can read, write and change.
  
  In this example, we see the use of the 'set' and 'modify' procedures, which
  require the \"set\" and \"modify\" attributes.
  
  __Example 3 (mutable 'IOField'):__
  
  Up to this point, we have been working with immutable fields.
  Now let's look at the mutable:
  
  @
    -- Create 'IORef' - mutable variable.
    ioref <- 'newIORef' (1 :: Int)
    
    -- Create mutable 'IOField' using 'field' procedure.
    myField <- 'field' 'iorefP' :: 'IO' ('IOField' '['ReadA', 'WriteA'] ('IORef' Int) Int)
    
    let getGetter    = 'use' (Proxy :: Proxy "get") (Proxy :: Proxy "get")
    let modifySetter = 'use' (Proxy :: Proxy "set") (Proxy :: Proxy "modify")
    
    -- same as 'get' myField ioref
    getter' <- getGetter myField
    getter' ioref
    
    modifySetter myField $ \\ setter' a -> setter' (max 0 a)
    
    'set' myField ioref 1
    'get' myField ioref
    -- ^ 'return' 1
    
    'set' myField ioref (-5)
    'get' myField ioref
    -- ^ 'return' 0
  @
  
  __Example 4 (manual attributes):__
  
  Now let's create the field \"manually\":
  
  @
    fileSize' :: IsMVar IO var => Proxy var -> IO (IOField [GetA, SetA, ModifyA, ModifyMA] Handle Integer)
    fileSize' =  \\ var -> do
      getter <- 'var'' var $ 'AccessGet'    'System.IO.hFileSize'
      setter <- 'var'' var $ 'AccessSet' 'System.IO.hSetFileSize'
      
      modifier \<- 'var'' var . 'AccessModify' $ \\ hdl f -\> do
        size <- f \<$\> 'System.IO.hFileSize' hdl
        size <$ 'System.IO.hSetFileSize' hdl size
      
      modifierM \<- var' var . 'AccessModifyM' $ \\ hdl go -\> do
        size <- go =<< hFileSize hdl
        size <$ hSetFileSize hdl size
      
      'return' $ def \`'Field'\` 'SomeProp' ('Prop' 'Data.Default.Class.def' modifierM)
                   \`'Field'\` 'SomeProp' ('Prop' 'Data.Default.Class.def' modifier)
                   \`'Field'\` 'SomeProp' ('Prop' 'Data.Default.Class.def' setter)
                   \`'Field'\` 'SomeProp' ('Prop' 'Data.Default.Class.def' getter)
  @
  
  So we use the @var'@ function to specify the variable's type and make the
  field from created attributes.
  
  __Example 5 (custom attributes):__
  
  As we saw in the previous example, manually creating new fields is a headache.
  Now let's do it the right way:
  
  @
  instance 'Attribute' "get" "" 'IO' 'System.IO.Handle' Integer
    where
      'attribute' = 'AccessGet' 'System.IO.hFileSize'
  
  instance 'Attribute' "set" "" 'IO' 'System.IO.Handle' Integer
    where
      'attribute' = 'AccessSet' 'System.IO.hSetFileSize'
  
  instance 'Attribute' "modify" "" 'IO' 'System.IO.Handle' Integer
    where
      'attribute' = 'AccessModify' $ \\ hdl f -> do
        size <- f \<$\> 'System.IO.hFileSize' hdl
        size <$ 'System.IO.hSetFileSize' hdl size
  
  instance 'Attribute' "modifyM" "" 'IO' 'System.IO.Handle' Integer
    where
      'attribute' = 'AccessModifyM' $ \\ hdl go -> do
        size <- go =<< 'System.IO.hFileSize' hdl
        size <$ 'System.IO.hSetFileSize' hdl size
  
  -- | With 'Attribute' instances we can create default attributes manually or just use 'field'.
  fileSize' :: IsMVar IO var => Proxy var -> 'IO' ('IOField' ['GetA', 'SetA', 'ModifyA', 'ModifyMA'] 'System.IO.Handle' Integer)
  fileSize' =  \\ p# -> do
    getter    <- var' p# 'attribute'
    setter    <- var' p# 'attribute'
    modifier  <- var' p# 'attribute'
    modifierM <- var' p# 'attribute'
    
    'return' $ 'Data.Default.Class.def' \`'Field'\` 'SomeProp' ('Prop' 'Data.Default.Class.def' modifierM)
                 \`'Field'\` 'SomeProp' ('Prop' 'Data.Default.Class.def' modifier)
                 \`'Field'\` 'SomeProp' ('Prop' 'Data.Default.Class.def' setter)
                 \`'Field'\` 'SomeProp' ('Prop' 'Data.Default.Class.def' getter)
  @
  
  __Example 6 (custom props):__
  
  Previously, we used only predefined attributes. Now that we know how to create
  predefined ones, let's create a new attributes. Another unpleasant point: the
  type 'fromLabel' in @base >= 4.10@ and @base-4.9@ is different.
  
  Sub-attribute for \"get\".
  
  @
    type instance 'AccessUse' "get" "length" m rep a = rep -> m Int
    
    data instance 'AccessRep' "get" "length" m rep a
    
    instance 'Attribute' "get" "length" m rep a
      where
        'attribute' = undefined
    
    instance (Foldable t, IsMVar m var) => 'UseAttribute' var "get" "length" m rep (t a)
      where
        'useAttr' _ storedGetter rep = do
          'AccessGet' getter <- fromMRef storedGetter
          length \<$\> getter rep
    
    listLength' ::
      (
        IsMVar new var, IsMVar m var, Foldable t,
        'Attribute' "get" "" m rep (t a)
      ) => Proxy var -> new ('FieldT' m '['GetA'' '["length"]] rep (t a))
    listLength' =  field
  @
  
  Something completely different:
  
  @
    type instance 'AccessUse' "version" "" m rep a = m 'Data.Version.Version'
    
    newtype instance 'AccessRep' "version" "" m rep a = AccessVersion
      {
        accessVersion :: 'Data.Version.Version'
      }
    
    instance IsMVar m var => 'UseAttribute' var "version" "" m rep a
      where
        'useAttr' _ versionRep = accessVersion \<$\> 'fromMRef' versionRep
  @
  
  So now we can define a new attribute or sub-attribute.
  
  __Example 7 (overloaded labels):__
  
  You can also use @OverloadedLabels@. __Example 1__ would look like this:
  
  @
    runIdentity $ do
     idField <- field identityP :: Identity (FieldT Identity '[GetA] Int Int)
     #get idField (1 :: Int) :: Identity Int -- return 1
  @
  
  As you can see, in general OverloadedLabels requires more explicit types.
  In addition, 'IsLabel' does not allow you to add an instance with 'AccessUse',
  so for each new field you will have to add a new instances.
  
  __Restrictions:__

  Unfortunately, @fmr-0.3@ still has a number of limitations:
  
  * fmr-0.3 requires more explicit types than @fmr-0.2@
  * 'Data.Typeable.Typeable' and 'Data.Typeable.cast' is no longer used in @fmr-0.3@
  * attributes of one Prop must be stored in variables of the same type
  * patterns from older versions can only be used as functions now
  * there is no pretty way to insert or replace an attribute in an 'field'-generated
  'Prop' or 'FieldT', only 'updateAttribute' and manual construction
  * there is no pretty way to create a field from a list of prepared attributes
  because they have different types
  * additional type wrappers in 'AccessRep' to simplify function signatures,
  which make code more complicated
  * 'FieldT' is not an object and store only attributes (procedures and metadata)
  * 'FieldT'-derived types (such as newtype or wrapper structures) willn't be
  automatically compatible with the library
  * field creation and operations with it can occur in different monads, which
  is not always convenient and may require additional types to be specified
  * in general, creating fields in the new version is more difficult than in the
  old version, although working with existing ones has become much easier
  * due to type system restrictions, recursive operations on 'FieldT', 'SomeProp'
  and 'Prop' types have to be implemented through private type classes, which
  leads to "deeply magical" constraint types.
-}

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'Field' - field with actions inside 'Identity' monad.
-}
type Field = FieldT Identity

{- |
  @since 0.3
  
  'STField' - field with actions inside 'ST' monad.
-}
type STField s = FieldT (STRef s)

{- |
  @since 0.3
  
  'IOField' - field with actions inside 'IO' monad.
-}
type IOField = FieldT IO

{- |
  @since 0.3
  
  'TField' - field with actions inside 'STM' monad.
-}
type TField = FieldT STM

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Apply actions to given value and discard results.
-}
sets :: Monad m => rep -> [RunFieldT m rep] -> m ()
sets =  mapM_ . flip ($)

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
  
  Getter 'Attr'.
-}
type GetA' subs = Attr' "get" subs

{- |
  @since 0.3
  
  Setter 'Attr'.
-}
type SetA' subs = Attr' "set" subs

{- |
  @since 0.3
  
  Modifier 'Attr'.
-}
type ModifyA' subs = Attr' "modify" subs

{- |
  @since 0.3
  
  Monadic modifier 'Attr'.
-}
type ModifyMA' subs = Attr' "modifyM" subs

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Constant getter 'Attr'.
-}
type GetA = GetA' '[]

{- |
  @since 0.3
  
  Constant setter 'Attr'.
-}
type SetA = SetA' '[]

{- |
  @since 0.3
  
  Constant modifier 'Attr'.
-}
type ModifyA = ModifyA' '[]

{- |
  @since 0.3
  
  Constant monadic modifier 'Attr'.
-}
type ModifyMA = ModifyMA' '[]

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
  
  Mutable monadic modifier 'Attr'.
-}
type ReadA = GetA' '["get", "set", "modify", "modifyM"]

{- |
  @since 0.3
  
  Mutable monadic modifier 'Attr'.
-}
type WriteA = SetA' '["get", "set", "modify", "modifyM"]

{- |
  @since 0.3
  
  Mutable monadic modifier 'Attr'.
-}
type UpdateA = ModifyA' '["get", "set", "modify", "modifyM"]

{- |
  @since 0.3
  
  Mutable monadic modifier 'Attr'.
-}
type UpdateMA = ModifyMA' '["get", "set", "modify", "modifyM"]

--------------------------------------------------------------------------------

-- | One-way pattern @(field ':=' value) === 'set' field value@.
pattern (:=) :: (Monad m, UseField "set" "" api)
             => FieldT m api rep a -> a
             -> RunFieldT m rep

pattern fld := a <- (const Nothing -> Just (fld, a))
  where
    fld := a = \ rep -> set fld rep a

-- | One-way pattern @(field ':~' value) === 'modify' field value@.
pattern (:~) :: (Monad m, UseField "modify" "" api)
             => FieldT m api rep a -> (a -> a)
             -> RunFieldT m rep

pattern fld :~ a <- (const Nothing -> Just (fld, a))
  where
    fld :~ a = \ rep -> void $ modify fld rep a

-- | One-way pattern @(field '::=' f) === let value = f field in 'modify' field value@
pattern (::=) :: (Monad m, UseField "set" "" api)
              => FieldT m api rep a -> (FieldT m api rep a -> a)
              -> RunFieldT m rep

pattern fld ::= f <- (const Nothing -> Just (fld, f))
  where
    fld ::= f = fld := f fld

-- | One-way pattern @(field '::~' f) === let g = f field in 'modify' field g@
pattern (::~) :: (Monad m, UseField "modify" "" api)
              => FieldT m api rep a -> (FieldT m api rep a -> a -> a)
              -> RunFieldT m rep

pattern fld ::~ f <- (const Nothing -> Just (fld, f))
  where
    fld ::~ f = fld :~ f fld

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Set value if any.
-}
(?=) :: (Monad m, UseField "set" "" api)
     => FieldT m api rep a -> Maybe a
     -> RunFieldT m rep

fld ?= a = \ rep -> maybe (return ()) (set fld rep) a

{- |
  @since 0.3
  
  Modify value if any.
-}
(?~) :: (Monad m, UseField "modify" "" api)
     => FieldT m api rep a -> (a -> Maybe a)
     -> RunFieldT m rep

fld ?~ f = \ rep -> () <$ modify fld rep (\ x -> x `fromMaybe` f x)

{- |
  @since 0.3
  
  Set value if any.
-}
(??=) :: (Monad m, UseField "set" "" api)
      => FieldT m api rep a -> (FieldT m api rep a -> Maybe a)
      -> RunFieldT m rep

fld ??= f = fld ?= f fld

{- |
  @since 0.3
  
  Modify value if any.
-}
(??~) :: (Monad m, UseField "modify" "" api)
      => FieldT m api rep a -> (FieldT m api rep a -> a -> Maybe a)
      -> RunFieldT m rep

fld ??~ f = fld ?~ f fld

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  @(':=')@ for many field.
-}
($:=) :: (Monad m, UseField "set" "" api)
      => [FieldT m api rep a] -> a
      -> RunFieldT m rep

flds $:= a = \ rep -> flds `forM_` \ fld -> fld := a $ rep

{- |
  @since 0.3
  
  @(':~')@ for many field.
-}
($:~) :: (Monad m, UseField "modify" "" api)
      => [FieldT m api rep a] -> (a -> a)
      -> RunFieldT m rep

flds $:~ f = \ rep -> flds `forM_` \ fld -> fld :~ f $ rep

{- |
  @since 0.3
  
  @('::=')@ for many field.
-}
($::=) :: (Monad m, UseField "set" "" api)
       => [FieldT m api rep a] -> (FieldT m api rep a -> a)
       -> RunFieldT m rep

flds $::= f = \ rep -> flds `forM_` \ fld -> fld ::= f $ rep

{- |
  @since 0.3
  
  @('::~')@ for many field.
-}
($::~) :: (Monad m, UseField "modify" "" api)
       => [FieldT m api rep a] -> (FieldT m api rep a -> a -> a)
       -> RunFieldT m rep

flds $::~ f = \ rep -> flds `forM_` \ fld -> (fld ::~ f) rep

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Set multiple value if any.
-}
(?:=) :: (Monad m, UseField "set" "" api)
      => [FieldT m api rep a] -> Maybe a
      -> RunFieldT m rep

fld ?:= a = \ rep -> maybe (return ()) (\ x -> fld $:= x $ rep) a

{- |
  @since 0.3
  
  Modify multiple value if any.
-}
(?:~) :: (Monad m, UseField "modify" "" api)
      => [FieldT m api rep a] -> (a -> Maybe a)
      -> RunFieldT m rep

fld ?:~ f = fld $:~ \ x -> x `fromMaybe` f x

{- |
  @since 0.3
  
  Set multiple value if any.
-}
(?::=) :: (Monad m, UseField "set" "" api)
       => [FieldT m api rep a] -> Maybe (FieldT m api rep a -> a)
       -> RunFieldT m rep

fld ?::= f = \ rep -> maybe (return ()) (\ g -> fld $::= g $ rep) f

{- |
  @since 0.3
  
  Modify multiple value if any.
-}
(?::~) :: (Monad m, UseField "modify" "" api)
       => [FieldT m api rep a] -> Maybe (FieldT m api rep a -> a -> a)
       -> RunFieldT m rep

fld ?::~ f = \ rep -> maybe (return ()) (\ g -> fld $::~ g $ rep) f


