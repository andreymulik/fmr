{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE Trustworthy, MagicHash, TypeOperators, TypeFamilies #-}
{-# LANGUAGE PolyKinds, DataKinds, ConstraintKinds #-}

{- |
    Module      :  Data.Field.Utils
    Copyright   :  (c) Andrey Mulik 2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "Data.Field.Utils" re-exports basic operations on variables.
-}
module Data.Field.Utils
(
  -- * IsMVar type family
  IsMVar (..), var', Identity (..),
  
  -- ** Basic reference interface
  Proxy#, proxy#, Proxy (..), toProxy#,
  
  -- * Uniqueness
  UniqueList, type (/=),
  
  -- * Variables
  -- ** STRef
  ST, STRef, newSTRef, readSTRef, writeSTRef, updateSTRef, updateMSTRef,
  
  -- ** IORef
  IORef, newIORef, readIORef, writeIORef, updateIORef, updateMIORef,
  
  -- ** MVar
  MVar, newMVar, readMVar, writeMVar, updateMVar, updateMMVar,
  
  -- ** TVar
  STM, TVar, newTVar, readTVar, writeTVar, updateTVar, updateMTVar
)
where

import Data.Functor.Identity
import Data.Type.Equality
import Data.Kind

import Data.Proxy
import Data.IORef
import Data.STRef

import GHC.TypeLits
import GHC.Exts ( Proxy#, proxy# )
import GHC.Conc

import Control.Monad
import Control.Monad.ST
import Control.Concurrent.MVar

default ()

--------------------------------------------------------------------------------

type family UniqueList xs :: Constraint
  where
    UniqueList   '[]    = ()
    UniqueList (x : xs) = (
        UniqueList xs, NotListElem x xs
        (
          TypeError
          (
            'Text "duplicate is found: " ':<>: 'ShowType x ':<>:
            'Text " in type list " ':<>: 'ShowType (x : xs)
          )
        )
      )

type family NotListElem x xs err :: Constraint
  where
    NotListElem _   '[]      _ = ()
    NotListElem x (x : xs) err = err
    NotListElem x (_ : xs) err = NotListElem x xs err

--------------------------------------------------------------------------------

type (x :: k) /= (y :: k) = (x == y) ~ 'False

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Class of variables which can be used as 'Data.Field.Prop.Prop' attribute
  representation storage.
-}
class Monad m => IsMVar m var
  where
    var :: value -> m (var value)
    
    fromMRef :: var value -> m value

instance Monad m => IsMVar m Identity
  where
    var  = return . return
    fromMRef = return . runIdentity

instance IsMVar (ST s) (STRef s)
  where
    var  = newSTRef
    fromMRef = readSTRef

instance IsMVar IO IORef
  where
    var  = newIORef
    fromMRef = readIORef

instance IsMVar IO MVar
  where
    var  = newMVar
    fromMRef = readMVar

instance IsMVar STM TVar
  where
    var  = newTVar
    fromMRef = readTVar

{- |
  @since 0.3
  
  'var'' is 'var' version with type hinting.
-}
var' :: IsMVar m var => Proxy var -> value -> m (var value)
var' =  const var

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Update for 'STRef'.
-}
{-# INLINE updateSTRef #-}
updateSTRef :: STRef s a -> (a -> a) -> ST s a
updateSTRef rep f = do val <- f <$> readSTRef rep; val <$ (writeSTRef rep $! val)

{- |
  @since 0.3
  
  Update for 'STRef'.
-}
{-# INLINE updateMSTRef #-}
updateMSTRef :: STRef s a -> (a -> ST s a) -> ST s a
updateMSTRef rep f = do val <- f =<< readSTRef rep; val <$ (writeSTRef rep $! val)

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Update for 'IORef'.
-}
{-# INLINE updateIORef #-}
updateIORef :: IORef a -> (a -> a) -> IO a
updateIORef rep f = atomicModifyIORef' rep (\ a -> let b = f a in (b, b))

{- |
  @since 0.3
  
  Update for 'IORef'.
-}
{-# INLINE updateMIORef #-}
updateMIORef :: IORef a -> (a -> IO a) -> IO a
updateMIORef rep go = do val <- go =<< readIORef rep; val <$ writeIORef rep val

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Write for 'MVar'.
-}
{-# INLINE writeMVar #-}
writeMVar :: MVar a -> a -> IO ()
writeMVar =  (void .) . swapMVar

{- |
  @since 0.3
  
  Update for 'MVar'.
-}
{-# INLINE updateMVar #-}
updateMVar :: MVar a -> (a -> a) -> IO a
updateMVar rep f = modifyMVar rep (\ a -> let b = f a in pure (b, b))

{- |
  @since 0.3
  
  Update for 'MVar'.
-}
{-# INLINE updateMMVar #-}
updateMMVar :: MVar a -> (a -> IO a) -> IO a
updateMMVar rep f = modifyMVar rep (\ a -> do b <- f a; pure (b, b))

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  Update for 'TVar'.
-}
{-# INLINE updateTVar #-}
updateTVar :: TVar a -> (a -> a) -> STM a
updateTVar rep f = do x <- f <$!> readTVar rep; x <$ writeTVar rep x

{- |
  @since 0.3
  
  Update for 'TVar'.
-}
{-# INLINE updateMTVar #-}
updateMTVar :: TVar a -> (a -> STM a) -> STM a
updateMTVar rep f = do x <- f =<< readTVar rep; x <$ writeTVar rep x

--------------------------------------------------------------------------------

toProxy# :: Proxy (a :: k) -> Proxy# (a :: k)
toProxy# =  \ _ -> proxy#


