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
  withAsyncLoader,
  newRequest,
  getLoadedResources,
  waitForResourceProcessing,
  AsyncLoader,
  ThreadWrapper(..),
  AsyncLoaderConfig(..)
  ) where

import Control.Monad
import Control.Exception (bracket)
import Control.Concurrent
import Control.Concurrent.STM

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
  stmProcessed :: TVar (Set l),
  stmResult :: TVar (LoaderResult l r),
  marshalThread :: ThreadId,
  workerThreads :: [ThreadId]
}

newtype ThreadWrapper l r u e = ThreadWrapper { getThreadWrapper :: u -> (LoadUnloadCallbacks l r e -> IO ()) -> LoadUnloadCallbacks l r e -> IO ThreadId }

data AsyncLoaderConfig l r u e = AsyncLoaderConfig {
  callbackFunctions :: LoadUnloadCallbacks l r e,
  workerCount :: Int,
  forkMarshal :: ThreadWrapper l r u e,
  forkLoadWorker :: ThreadWrapper l r u e
}

startAsyncLoader :: (Ord l) => AsyncLoaderConfig l r u e -> u -> IO (AsyncLoader l r e)
startAsyncLoader config userData = do
  stmIn <- newTVarIO (LoaderRequest S.empty)
  stmInter <- newTVarIO (IntermediateResourceMap M.empty M.empty M.empty M.empty)
  stmProcess <- newTVarIO S.empty
  stmOut <- newTVarIO (LoaderResult M.empty)
  let callbacks = callbackFunctions config
  let marshalWrapper = getThreadWrapper (forkMarshal config)
  let workerWrapper = getThreadWrapper (forkLoadWorker config)
  marshaller <- marshalWrapper userData (marshalGo stmIn stmInter stmProcess stmOut) callbacks
  let workerInits = replicate (workerCount config) (workerGo stmInter stmProcess)
  workers <- mapM (\g -> workerWrapper userData g callbacks) workerInits
  return $ AsyncLoader stmIn stmInter stmProcess stmOut marshaller workers

shutdownAsyncLoader :: AsyncLoader l r e -> IO ()
shutdownAsyncLoader loader = do
  --print "Shutting down"
  -- set to empty
  atomically $ writeTVar (stmRequest loader) (LoaderRequest S.empty)
  -- wait for the output to go down to null
  drainWorkers loader
  -- everything is unloaded, kill the threads
  killThread (marshalThread loader)
  mapM_ killThread (workerThreads loader)
    where
      drainWorkers ld = atomically $ do
          (LoaderResult r) <- readTVar (stmResult ld)
          unless (M.null r) retry

withAsyncLoader :: (Ord l) => AsyncLoaderConfig l r u e -> u -> ((AsyncLoader l r e) -> IO x) -> IO x
withAsyncLoader config userData wrapped = bracket createLoader destroyLoader wrapped
  where
    createLoader = startAsyncLoader config userData
    destroyLoader = shutdownAsyncLoader

-- | Put in a new set of resources for the loader to sync with
newRequest :: AsyncLoader l r e -> Set l -> IO ()
newRequest loader newTopSet = do
  let reqVar = stmRequest loader
  atomically $ writeTVar reqVar (LoaderRequest newTopSet)

-- | Get the set of loaded resources. This returns instantly, just returning whatever is currently loaded
getLoadedResources :: AsyncLoader l r e -> IO (Map l r)
getLoadedResources loader = getLoaderResult <$> readTVarIO (stmResult loader)

-- | Load in a new set of resources and waits for the processing to complete.  Note that you need to provide both the old and new sets
--   as a tuple @(oldSet, newSet)@ so that both load and unloads can be properly checked
waitForResourceProcessing :: (Ord l) => AsyncLoader l r e -> (Set l, Set l) -> IO (Map l r)
waitForResourceProcessing loader (oldSet, newSet) = do
  -- we need to compute which things get unloaded so we can wait for them to vanish
  let unloads = S.difference oldSet newSet
  newRequest loader newSet
  atomically $ do
    p <- getLoaderResult <$> readTVar (stmResult loader)
    let pSet = S.fromList (M.keys p)
    let remainingUnloads = S.intersection unloads pSet
    unless (newSet `S.isSubsetOf` pSet) retry
    unless (S.null remainingUnloads) retry
    return p

-- | Internal process to get user requests and convert them into something the workers can use to load/unload
marshalGo :: (Ord l) => TVar (LoaderRequest l r) -> TVar (IntermediateResourceMap l r e) -> TVar (Set l) -> TVar (LoaderResult l r) -> LoadUnloadCallbacks l r e-> IO ()
marshalGo reqT interT processedT resultT callbacks = loaderLoop S.empty M.empty
  where
    loaderLoop oldTop oldResources = do
      -- repeatedly check and wait (via retry) for new data in the request TVar
      newTop <- atomically $ do
        (LoaderRequest requested) <- readTVar reqT
        if requested == oldTop
          then retry
          else return requested

      -- figure out what needs to be loaded/unloaded and put into the intermediate TVar for worker threads to examine
      newIntermediate <- findLoadsUnloads callbacks (ResourcesMap oldResources oldTop) newTop
      atomically $ do
        writeTVar interT newIntermediate
        writeTVar processedT S.empty

      let totalProcessed = S.fromList $ M.keys (loadingSet newIntermediate) ++ M.keys (unloadingSet newIntermediate)

      -- wait until all resources have been processed by the worker threads
      newResources <- atomically $ do
        (IntermediateResourceMap loaded loading unloading _) <- readTVar interT
        -- first the loading/unloading sets need to be empty
        unless (M.null loading && M.null unloading) retry
        -- all things must be processed
        processed <- readTVar processedT
        -- wait until all things have been processed
        unless (processed == totalProcessed) retry
        return loaded

      let resultValue = LoaderResult $ M.map value newResources   -- strip out the 'ResourceInfo' and keep the value
      atomically $ writeTVar resultT resultValue
      loaderLoop newTop newResources


data WorkerAction l r = WorkerLoad l (Map l r) | WorkerUnload (ResourceInfo l r)

workerGo :: (Ord l) => TVar (IntermediateResourceMap l r e) -> TVar (Set l) -> LoadUnloadCallbacks l r e -> IO ()
workerGo interT processedT callbacks = forever (findNextAction >>= processAction)
    where
      -- look for something to load or unload
      findNextAction = atomically $ do
        inter@(IntermediateResourceMap loaded loading unloading _) <- readTVar interT
        case nextUnload unloading loaded of
          Just ul -> do
            writeTVar interT $ inter { unloadingSet = M.delete (loadKey ul) (unloadingSet inter) }
            return $ WorkerUnload ul
          Nothing -> case nextLoad loading loaded of
            Just (ResourceInfo l _ deps ) -> do
              case pullDeps loaded deps of
                Just depMap -> do
                  writeTVar interT $ inter { loadingSet = M.delete l (loadingSet inter) }
                  return $ WorkerLoad l depMap
                Nothing -> undefined -- need to handle error here
            -- there is nothing to load/unload, which means there is nothing left to process or we need to wait
            -- until other resources have been processed. In either case, we wait until another thread updates the TVar
            -- and then check again
            Nothing -> retry
      -- fired off the load or unload action, as appropriate
      processAction nextAction =
        case nextAction of
          WorkerLoad l depMap -> do
            loadResult <- loadResource callbacks l depMap
            let deps = S.fromList (M.keys depMap)
            atomically $ do
              modifyTVar interT $ \inter -> inter { currentlyLoaded = M.insert l (ResourceInfo l loadResult deps) (currentlyLoaded inter) }
              modifyTVar processedT $ S.insert l
          WorkerUnload ul -> do
            unloadResource callbacks ul
            atomically $ do
              let l = loadKey ul
              modifyTVar interT $ \inter -> inter { currentlyLoaded = M.delete l (currentlyLoaded inter) }
              modifyTVar processedT $ S.insert l

