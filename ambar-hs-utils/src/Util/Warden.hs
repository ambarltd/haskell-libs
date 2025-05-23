module Util.Warden
  ( Warden
  , withWarden
  , spawn
  , spawn_
  , spawnMask
  , spawnLinked
  , spawnLinkedMask
  ) where

import Control.Concurrent
  ( MVar
  , modifyMVar
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , swapMVar
  , takeMVar
  , tryPutMVar
  , putMVar
  )
import Control.Concurrent.Async
  ( Async
  , AsyncCancelled(..)
  , asyncWithUnmask
  , cancel
  , mapConcurrently
  , withAsync
  , concurrently_
  )
import Control.Exception
  ( SomeException
  , bracket
  , catch
  , finally
  , fromException
  , mask_
  , throwIO
  , uninterruptibleMask_
  )
import Control.Monad (forM_, void)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import System.IO (fixIO)

import Util.Async (withAsyncThrow)
import Util.Delay (hang)

-- | A Warden is an owner of Asyncs which cancels them on shutdown
-- and can propagate exceptions from children to parent.
--
-- 'Nothing' in the MVar means the 'Warden' has been shut down.
data Warden = Warden
  { childException :: MVar SomeException
  , _asyncs :: MVar (Maybe (HashSet (Async ())))
  }

withWarden :: (Warden -> IO a) -> IO a
withWarden f = bracket create shutdown $ \warden ->
  withAsyncThrow (monitor warden) $ f warden
  where
  create :: IO Warden
  create = Warden <$> newEmptyMVar <*> newMVar (Just mempty)

  shutdown :: Warden -> IO ()
  shutdown (Warden _ v) = do
    masyncs <- uninterruptibleMask_ $ swapMVar v Nothing
    forM_ masyncs $ mapConcurrently cancel . HashSet.toList

  monitor :: Warden -> IO ()
  monitor (Warden evar _) =
    concurrently_
      (throwIO =<< takeMVar evar)       -- throw child exception
      (hang <* putMVar evar undefined)  -- prevent blocked indefinitely on mvar

spawnMask :: Warden -> ((forall b. IO b -> IO b) -> IO a) -> IO (Async a)
spawnMask (Warden _ v) act =
  modifyMVar v $ \case
    Nothing ->
      withAsync (throwIO AsyncCancelled) $ \a ->
        return (Nothing, a)
    Just as -> do
      a <- fixIO $ \a -> mask_ $
        asyncWithUnmask $ \unmask ->
        act unmask `finally` forget a
      return (Just $ HashSet.insert (void a) as, a)
  where
  forget a = modifyMVar_ v $ return . fmap (HashSet.delete (void a))

spawn :: Warden -> IO a -> IO (Async a)
spawn w act = spawnMask w $ \unmask -> unmask act

spawn_ :: Warden -> IO a -> IO ()
spawn_ w act = void $ spawn w act

-- | Make a thread's exceptions seize-up the entire Warden computation.
spawnLinkedMask :: Warden -> ((forall b. IO b -> IO b) -> IO a) -> IO (Async a)
spawnLinkedMask w f = spawnMask w $ \unmask -> f unmask `catch` throwUp
  where
  throwUp :: SomeException -> IO a
  throwUp ex
    | Just AsyncCancelled <- fromException ex = throwIO ex
    | otherwise = do
      -- we use tryPutMVar here because we don't want the thread to be
      -- stuck if it receives an exception during warden shutdown.
      void $ tryPutMVar (childException w) ex
      throwIO ex

spawnLinked :: Warden -> IO a -> IO (Async a)
spawnLinked w act = spawnLinkedMask w $ \unmask -> unmask act
