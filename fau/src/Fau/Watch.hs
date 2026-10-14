-- | Re-running an action when files change.
module Fau.Watch (watchFiles) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar, tryTakeMVar)
import Control.Exception (SomeAsyncException, SomeException, catch, displayException, fromException, throwIO)
import Control.Monad (forever, void, when)
import System.FSNotify (eventPath, watchTree, withManager)
import System.IO (hPutStrLn, stderr)

-- | Run @action@, rerun on change in @relevant@, print errors
watchFiles :: FilePath -> (FilePath -> IO Bool) -> IO () -> IO a
watchFiles root relevant action = withManager $ \mgr -> do
    changed <- newEmptyMVar
    _ <- watchTree mgr root (const True) $ \event -> do
        yes <- relevant (eventPath event) `catch` \(_ :: SomeException) -> pure False
        when yes $ void (tryPutMVar changed ())
    forever $ do
        withAsync (action `catch` report) $ \_ -> takeMVar changed
        -- wait for save to finish
        threadDelay 100000
        void (tryTakeMVar changed)
  where
    report e = case fromException e of
        Just (_ :: SomeAsyncException) -> throwIO e
        Nothing -> hPutStrLn stderr ("fau: " <> displayException (e :: SomeException))
