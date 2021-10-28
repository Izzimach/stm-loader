{-|
Module      : SyncLoader
Description : Start up/fork an asynchronous loader of resources.
Copyright   : (c) Gary Haussmann 2021
Stability   : experimental

Runs a separate thread(s) to load resources. Access is via STM variables:
- you put in a set of load keys in the 'stmRequest' variable
- resources are loaded and unloaded to match the input set of requests
- once complete, a map of the loaded resources is put in the 'stmResult' variable

-}


{-# LANGUAGE BangPatterns #-}

module STMLoader.AsyncLoader (
  startAsyncLoader,
  shutdownAsyncLoader,
  newRequest,
  getLoadedResources,
  waitForLoadedResources,
  AsyncLoader,
  ForkWrapper(..),
  AsyncLoaderConfig(..)
  ) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async.Pool

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import STMLoader.LoadUnload



newtype LoaderRequest l r = LoaderRequest { getLoaderRequest :: Set l } deriving (Eq, Show)
newtype LoaderResult l r = LoaderResult { getLoaderResult :: Map l r } deriving (Eq, Show)

data AsyncLoader l r e = AsyncLoader {
  stmRequest :: TVar (LoaderRequest l r),
  stmIntermediate :: TVar (IntermediateResourceMap l r e),
  stmResult :: TVar (LoaderResult l r),
  loaderThread :: ThreadId,
  workerThreads :: [ThreadId]
}

newtype ForkWrapper l r u e = ForkWrapper { getForkWrapper :: u -> (LoadUnloadConfig l r e -> IO ()) -> LoadUnloadConfig l r e -> IO ThreadId }

data AsyncLoaderConfig l r u e = AsyncLoaderConfig {
  loadUnloadFunctions :: LoadUnloadConfig l r e,
  workerCount :: Int,
  forkMarshal :: ForkWrapper l r u e,
  forkLoadWorker :: ForkWrapper l r u e
}

startAsyncLoader :: (Ord l) => AsyncLoaderConfig l r u e -> u -> IO (AsyncLoader l r e)
startAsyncLoader config userData = do
  stmIn <- newTVarIO (LoaderRequest S.empty)
  stmInter <- newTVarIO (IntermediateResourceMap M.empty M.empty M.empty M.empty)
  stmOut <- newTVarIO (LoaderResult M.empty)
  let loadUnloadConfig = loadUnloadFunctions config
  let marshalWrapper = getForkWrapper (forkMarshal config)
  let workerWrapper = getForkWrapper (forkLoadWorker config)
  loaderThread <- marshalWrapper userData (marshalGo stmIn stmInter stmOut) loadUnloadConfig
  workerThread <- workerWrapper userData (workerGo stmInter) loadUnloadConfig
  return $ AsyncLoader stmIn stmInter stmOut loaderThread [workerThread]

shutdownAsyncLoader :: AsyncLoader l r e -> IO ()
shutdownAsyncLoader fr = do
  -- swap in an empty request
  atomically $ writeTVar (stmRequest fr) (LoaderRequest S.empty)
  -- wait for the output to go down to null
  atomically $ do
    outputs <- readTVar (stmResult fr)
    unless (M.null $ getLoaderResult outputs) retry
  -- everything is unloaded, kill the thread
  killThread (loaderThread fr)

-- | Put in a new set of resources for the loader to sync with
newRequest :: AsyncLoader l r e -> Set l -> IO ()
newRequest loader newTopSet = do
  let reqVar = stmRequest loader
  atomically $ writeTVar reqVar (LoaderRequest newTopSet)

-- | Get the set of loaded resources. This returns instantly, just returning whatever is currently loaded
getLoadedResources :: AsyncLoader l r e -> IO (Map l r)
getLoadedResources loader = getLoaderResult <$> readTVarIO (stmResult loader)

-- | Pass in a set of required resources. This waits (via 'retry') until all those resources are loaded, like a synchronous loader
waitForLoadedResources :: (Ord l) => AsyncLoader l r e -> Set l -> IO (Map l r)
waitForLoadedResources loader required =
  atomically $ do
    (LoaderResult curResult) <- readTVar (stmResult loader)
    if curResult `containsAll` required
    then return curResult
    else retry
    where
      containsAll a b = all (\k -> M.member k a) b



-- | Internal process to get user requests and convert them into something the workers can use to load/unload
marshalGo :: (Ord l) => TVar (LoaderRequest l r) -> TVar (IntermediateResourceMap l r e) -> TVar (LoaderResult l r) -> LoadUnloadConfig l r e-> IO ()
marshalGo reqT interT resultT config = loaderLoop (LoaderRequest S.empty) noResources
  where
    loaderLoop oldRequest curResources = do
      newRequest@(LoaderRequest newTop) <- atomically $ do
        newRequest <- readTVar reqT
        -- if the request hasn't changed, sleep until the request changes again
        if newRequest == oldRequest
          then retry
          else return newRequest
      newResources <- syncNewResources config curResources newTop
      let lResult = LoaderResult $ M.map value (resourcesMap newResources)
      atomically $ writeTVar resultT lResult
      loaderLoop newRequest newResources

workerGo :: (Ord l) => TVar (IntermediateResourceMap l r e) -> LoadUnloadConfig l r e -> IO ()
workerGo stmInter config = do
  return ()
