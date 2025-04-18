{-# LANGUAGE RecursiveDo #-}
{- |
A module that allows you to initialise a resource on demand and reutilise it
across different invocations.

If you have a function such as

  withResource :: (Resource -> IO b) -> IO b

It will initialise the resource on every invocation. To share an instance
across invocations one would have to wrap all invocations in a single
`withResource`, but that would cause the resource to be initialised regardless
of whether it is used or not.

This module allows on-demand initialisation and instance sharing.
The resource is released when it can't be accessed any longer.

The pattern is:

  -- nothing happens
  onDemand <- OnDemand.lazy withResource

  -- instance is initialised
  OnDemand.with onDemand $ \resource -> ...

  -- instance is reused
  OnDemand.with onDemand $ \resource -> ...
  OnDemand.with onDemand $ \resource -> ...

  -- instance is cleared

-}
module Util.OnDemand
  ( OnDemand
  , lazy
  , withLazy
  , with
  -- for testing
  , lazy_
  ) where

import Prelude hiding (init)
import Control.Concurrent.Async (Async, wait, cancel, async)
import Control.Concurrent.STM
import Control.Exception (BlockedIndefinitelyOnSTM(..), bracket, handle, finally)
import Control.Monad (when, forM_)
import System.Mem.Weak (deRefWeak)

import Util.Async (withAsyncThrow)

data OnDemand a = OnDemand (Async ()) (TMVar a) (TVar Int)

with :: OnDemand a -> (a -> IO b) -> IO b
with (OnDemand t var refs) f =
  withAsyncThrow (wait t) $ bracket acquire release f
  where
  acquire = do
    atomically $ modifyTVar refs succ
    atomically $ readTMVar var

  release _ = do
    atomically $ modifyTVar refs pred

-- | A function of the pattern `withResource`
type Initializer a = (forall b. (a -> IO b) -> IO b)

lazy_ :: Initializer a -> IO (Async (), OnDemand a)
lazy_ init = mdo
  alive <- newTVarIO True -- live state of OnDemand
  refs <- newTVarIO 0     -- resource references
  var  <- newEmptyTMVarIO
  wvar <- mkWeakTMVar var $ atomically $ modifyTVar alive (const False)
  t <- async (initialise wvar alive refs)
  return (t, OnDemand t var refs)
  where
  waitForDemand aliveVar refsVar =
    handle (\BlockedIndefinitelyOnSTM -> return False) $ do
      atomically $ do
        demand <- readTVar refsVar
        alive <- readTVar aliveVar
        when (alive && demand == 0) retry
        return (demand > 0)

  waitForCompletion aliveVar refsVar =
    handle (\BlockedIndefinitelyOnSTM -> return ()) $
      atomically $ do
        alive <- readTVar aliveVar
        demand <- readTVar refsVar
        when (alive || demand > 0) retry

  initialise wvar alive refs = do
    -- wait till first 'with'
    hasDemand <- waitForDemand alive refs
    when hasDemand $ do
      init $ \resource -> do
        mvar <- deRefWeak wvar
        forM_ mvar $ \var -> do
          atomically $ putTMVar var resource
          -- wait till last 'with'
          waitForCompletion alive refs

lazy :: Initializer a -> IO (OnDemand a)
lazy init = snd <$> lazy_ init

-- | A version of lazy that doesn't wait for the garbage collector
-- to finish the resource thread.
-- before returning we return.
withLazy :: Initializer a -> (OnDemand a -> IO b) -> IO b
withLazy init act = do
  (thread, od) <- lazy_ init
  act od `finally` cancel thread
