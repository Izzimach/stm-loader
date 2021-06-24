{-|
Module      : SyncLoader
Description : Start up/fork an asynchronous loader of resources.
Copyright   : (c) Gary Haussmann 2021
Stability   : experimental

Runs a separate thread(s) to loade resources. Access is via STM variables:
- you put in a set of load keys in the 'stmRequest' variable
- resources are loaded and unloaded to match the input set of requests
- once complete, a map of the loaded resources is put in the 'stmResult' variable

-}


{-# LANGUAGE BangPatterns #-}

module AsyncLoader (
  forkLoader,
  shutdownLoader,
  ForkLoaderResult(..),
  LoaderRequest(..),
  LoaderResult(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async.Pool

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import LoadUnload

data ForkLoaderResult i o = ForkLoaderResult {
  stmRequest :: TVar i,
  stmResult :: TVar o,
  loaderThread :: ThreadId
}

newtype LoaderRequest l r = LoaderRequest { getLoaderRequest :: (S.Set l) } deriving (Eq, Show)

newtype LoaderResult l r = LoaderResult { getLoaderResult :: (M.Map l r) } deriving (Eq, Show)

forkLoader :: (Ord l, Show l) => Int -> ResourceLoaderConfig l r -> IO (ForkLoaderResult (LoaderRequest l r) (LoaderResult l r))
forkLoader n config = do
  stmIn <- atomically $ newTVar (LoaderRequest S.empty)
  stmOut <- atomically $ newTVar (LoaderResult M.empty)
  forkThread <- forkIO $ loaderGo n stmIn stmOut config
  return $ ForkLoaderResult stmIn stmOut forkThread

loaderGo :: (Ord l, Show l) => Int -> TVar (LoaderRequest l r) -> TVar (LoaderResult l r) -> ResourceLoaderConfig l r -> IO ()
loaderGo groupSize reqT resultT config = withTaskGroup groupSize $ \tg -> loaderLoop tg (LoaderRequest S.empty) noLoadedResources
  where
    loaderLoop tg oldReq loaded = do
      requested@(LoaderRequest newReq) <- atomically $ do
        newReq <- readTVar reqT
        if (newReq == oldReq)
           then retry
           else return newReq
      let toLoad = newReq `S.difference` (topLevelResources loaded)
      let toUnload = (topLevelResources loaded) `S.difference` newReq
      -- load stuff
      loaded' <- fullLoad tg config loaded (S.toList toLoad)
      -- unload stuff
      loaded'' <- fullUnload tg config loaded' (S.toList toUnload)
      let lResult = LoaderResult $ M.map value (resourceMap loaded'')
      atomically $ writeTVar resultT lResult
      loaderLoop tg requested loaded''

shutdownLoader :: ForkLoaderResult (LoaderRequest l r) (LoaderResult l r) -> IO ()
shutdownLoader fr = do
  -- swap in an empty request
  atomically $ writeTVar (stmRequest fr) (LoaderRequest S.empty)
  -- wait for all the outputs to go down to null
  atomically $ do
    outputs <- readTVar (stmResult fr)
    if (not $ M.null $ getLoaderResult outputs)
        then retry
        else return ()
  -- everything is unloaded, kill the thread
  killThread (loaderThread fr)
