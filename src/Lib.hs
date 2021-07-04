{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Lib (someFunc, loadTest, unloadTest, testLoaderConfig) where


import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async.Pool

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Data.Row

import LoadUnload
import AsyncLoader

type ResourceRow = ("a" .== Int) .+ ("b" .== String)
type SomeResource a = Var a


testLoaderConfig :: ResourceLoaderConfig String (SomeResource ResourceRow)
testLoaderConfig = 
  ResourceLoaderConfig
    {
      loadIO = \l _m -> do
        threadDelay (50000 + 10000 * length l)
        putStrLn ("loaded " ++ l)
        return (IsJust #b l)
    , unloadIO = \l _r -> do threadDelay (4000 + 4000 * (length l)); putStrLn ("delete " ++ l); return ()
    , dependenciesIO = \l -> do threadDelay 10000;
                                putStrLn ("depends " ++ l);
                                return $ if (l == "argh")
                                         then ["ack"]
                                         else if (l == "blargh")
                                         then ["ack","argh","forsooth"]
                                         else if (l == "ack")
                                         then ["elephants", "watermelon"]
                                         else []
    }



--res :: SomeResource ResourceRow
--res = IsJust #a 3


-- current recommended call: '@loadTest testLoaderConfig ["blargh"]
loadTest :: ResourceLoaderConfig String (SomeResource ResourceRow) -> [String] -> IO [SomeResource ResourceRow]
loadTest config ls = do
  let loaded = LoadedResources (M.fromList [("argh",ResourceInfo "argh" (IsJust #a 3) S.empty S.empty)]) S.empty
  loaded' <- withTaskGroup 4 $ \tg -> fullLoad tg config loaded ls
  putStrLn $ show loaded'
  return $ M.elems $ M.map value (resourceMap loaded')

syncLoadTest :: ResourceLoaderConfig String (SomeResource ResourceRow) -> [String] -> IO [SomeResource ResourceRow]
syncLoadTest config ls = do
  let loaded = LoadedResources (M.fromList [("argh",ResourceInfo "argh" (IsJust #a 3) S.empty S.empty)]) S.empty
  loaded' <- syncLoad config loaded ls
  putStrLn $ show loaded'
  return $ M.elems $ M.map value (resourceMap loaded')


-- current recommended call: @unloadTest testLoaderConfig ["blargh"]
unloadTest :: (Ord l, l ~ String, r ~ SomeResource ResourceRow) => ResourceLoaderConfig l r -> [String] -> IO ()
unloadTest config uls = do
  withTaskGroup 4 $ \tg -> do
    loaded <- fullLoad tg config (LoadedResources M.empty S.empty) ["blargh","ack"]
    loaded' <- fullUnload tg config loaded uls
    putStrLn $ show loaded'


asyncLoaderTest ::IO ()
asyncLoaderTest = do
  fr <- forkLoader 4 testLoaderConfig
  atomically $ writeTVar (stmRequest fr) (LoaderRequest $ S.fromList ["blargh"])
  load1 <- getLoads fr noResult
  putStrLn $ show load1
  atomically $ writeTVar (stmRequest fr) (LoaderRequest $ S.fromList ["ack"])
  load2 <- getLoads fr load1
  putStrLn $ show load2
  shutdownLoader fr
  return ()
    where
      noResult = LoaderResult M.empty

      getLoads fr oldResult = atomically $ do
        result <- readTVar (stmResult fr)
        if (result == oldResult)
          then retry
          else return result
  

{-


pullSTM :: (Patchable a) => TVar (ValDelta a) -> STM (ValDelta a)
pullSTM va = do
  prepatch <- readTVar va
  let postpatch = patch prepatch
  writeTVar va (valueBundle postpatch)
  return prepatch

forkResourceLoader :: (Show l, Ord l, Generic l, ValDeltaBundle r) => ResourceLoaderConfig l r -> IO (ThreadId, TVar (ValDeltaSet l), TVar (MVD.MapValDelta l r))
forkResourceLoader config = do
    (requests, loaded) <- atomically $ do
      r <- newTVar (ValDeltaSet (PatchableSet S.empty) (UpDownSet S.empty S.empty))
      l <- newTVar (MVD.MapValDelta M.empty)
      return (r,l)
    threadId <- forkIO (loaderThread requests loaded)
    return (threadId, requests, loaded)
  where
    loaderThread requests loaded = forever $ do
      -- get a set of stuff to load/unload from the "input" TVar
      rs <- atomically $ do
              rvd <- readTVar requests
              let (r,rd) = unbundleVD rvd
              let ii = inserts rd
              let dd = deletes rd
              if (S.null ii && S.null dd)
              then retry
              else do
                writeTVar requests (valueBundle (patch rvd))
                return rd

      let loads = (S.toList (inserts rs))
      let unloads = (S.toList (deletes rs))

      resources <- atomically $ readTVar loaded

      -- load things
      let loadThunk l = do
            r <- (loadIO config) l
            return (l,r)
      loadedList <- traverse loadThunk loads

      -- unload things
      let unloadPairs = mapMaybe
                          (\l -> case (MVD.lookup l resources) of
                                   Nothing -> Nothing
                                   Just r -> Just (l,r))
                          unloads
      traverse_ (unloadIO config) unloadPairs

      -- add the results in the "output" TVar
      atomically $ modifyTVar loaded $ \res ->
        let res' = foldl (\items (l,r) -> MVD.insert l r items) res loadedList
        in res'


testLoader :: IO ()
testLoader = do
  (tid, requestsTV, resultsTV) <- forkResourceLoader testLoaderConfig
  atomically $ modifyTVar requestsTV (\vv -> insert "argh" $ insert "ack" vv)
  threadDelay 500000
  atomically $ modifyTVar requestsTV (\vv -> delete "ack" $ insert "blorp" vv)
  threadDelay 1500000
  r <- atomically $ readTVar resultsTV
  putStrLn (show r)
-}













type BufferStore = TChan String

singleBuffer :: BufferStore -> IO ()
singleBuffer bufferStore = forever $
  do
    message <- atomically $ readTChan bufferStore
    putStrLn message
            
bufferPrint :: BufferStore -> String -> IO ()
bufferPrint bufferStore msg = atomically $ writeTChan bufferStore msg


someFunc :: IO ()
someFunc = do
  shared <- newTVarIO (0 :: Int)
  bufferStore <- atomically newTChan
  _thread1 <- forkIO $ printAtInterval shared bufferStore
  thread2 <- forkIO $ incrementAtInterval shared
  thread3 <- forkIO $ singleBuffer bufferStore
  threadDelay 200000
  killThread thread2
  killThread thread3


printAtInterval :: TVar Int -> BufferStore -> IO ()
printAtInterval pv bufferStore = replicateM_ 100 $
  do
    threadDelay 50000
    val <- atomically $ readTVar pv
    bufferPrint bufferStore (show val)

incrementAtInterval :: TVar Int -> IO ()
incrementAtInterval pv = forever $
  do
    threadDelay 9000
    atomically $ modifyTVar pv (+1)
