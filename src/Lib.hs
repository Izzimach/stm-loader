{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Lib
  ( someFunc
  ) where

import qualified GHC.Generics as GHC
import Generics.SOP

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable
import Data.Row
import Data.Row.Variants (view)
import Data.Maybe (mapMaybe)

import Zorja.Patchable
import Zorja.Collections.PatchableSet
import qualified Zorja.Collections.MapValDelta as MVD

type ResourceRow = ("a" .== Int) .+ ("b" .== String)
type SomeResource a = Var a


res :: SomeResource ResourceRow
res = IsJust #a 3

newtype ResourceMap l r = ResourceMap (M.Map l r)

type ResourceLoad l r = l -> IO r
type ResourceUnload l r = (l,r) -> IO ()


data ResourceLoaderConfig l r = ResourceLoaderConfig
  {
    loadIO :: ResourceLoad l r,
    unloadIO :: ResourceUnload l r
  }



{-
testLoaderConfig :: ResourceLoaderConfig String (SomeResource ResourceRow)
testLoaderConfig = 
  ResourceLoaderConfig
    {
      loadIO = \p -> do threadDelay 50000; putStrLn ("add " ++ p); return (IsJust #a 3),
      unloadIO = \v -> do threadDelay 60000; putStrLn ("delete " ++ fst v); return ()
    }

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






loadExample :: SomeResource ResourceRow -> IO (Var ResourceRow)
loadExample (view #a -> Just n) = return $ IsJust #a 3
loadExample (view #b -> Just n) = return $ IsJust #b "argh"








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
  thread1 <- forkIO $ printAtInterval shared bufferStore
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
