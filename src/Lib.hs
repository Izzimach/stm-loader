{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Lib (testLoad, asyncTestLoad) where


import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async.Pool

-- the set and map types are imported directly, but everything else is qualified, since Set and Map both have things like 'empty' and 'member'
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map.Strict as M

import Data.Row

import STMLoader.LoadUnload
import STMLoader.AsyncLoader

--
-- some REPL testing stuff
--

type ErrorMessage = String
type UserData = ()

data SomeResource = A Int | B String
  deriving (Eq,Ord,Show)


--
-- example functions for a simple synchronous loader
--

simpleDeps :: SomeResource -> IO (Set SomeResource)
simpleDeps (A x) = do
  threadDelay (x * 1000)
  if x < 10 && x > 0 then return S.empty else return $ S.singleton (A (x-10))
simpleDeps (B s) = do
  if length s < 6
  then do
    threadDelay $ 10000 + 1000 *length s
    return $ S.singleton (A 12)
  else do
    threadDelay $ 20000 + 1000 * length s
    return $ S.fromList [A 2, B (drop 5 s)]

simpleLoad :: SomeResource -> Map SomeResource String ->  IO String
simpleLoad r depMap = do
  putStrLn $ "load " ++ show r ++ ", " ++ show depMap
  threadDelay $ 10000 * length (show r)
  let deps = S.fromList (M.keys depMap)
  case r of
    A n -> return $ show n
    B s -> return $ "ack" ++ show (length s)

simpleUnload :: ResourceInfo SomeResource String -> IO ()
simpleUnload (ResourceInfo k _ _) = do
  threadDelay $ 10000 * length (show k)
  putStrLn $ "unload " ++ show k
    
simpleError :: forall l r err. (Exception err, Show l) => LoadUnloadError l r err -> TriageError l ErrorMessage
simpleError (DepsFailed l e) = Report $ "Deps failed for resource " ++ show l ++ " with error: " ++ displayException e
simpleError (LoadFailed l e) = Report $ "Load failed for resource " ++ show l ++ " with error: " ++ displayException e
simpleError (UnloadFailed l _ e) = Report $ "Unload failed for resource " ++ show l ++ " with error: " ++ displayException e


syncLoaderConfig :: LoadUnloadCallbacks SomeResource String ErrorMessage
syncLoaderConfig = LoadUnloadCallbacks simpleDeps simpleLoad simpleUnload simpleError

simpleFork :: ThreadWrapper SomeResource String UserData ErrorMessage
simpleFork = ThreadWrapper (\_ p config -> forkIO (p config))

asyncLoaderConfig :: AsyncLoaderConfig SomeResource String UserData ErrorMessage
asyncLoaderConfig = AsyncLoaderConfig
  {
    callbackFunctions = syncLoaderConfig,
    workerCount = 1,
    forkMarshal = simpleFork,
    forkLoadWorker = simpleFork
  }

loadSet1 :: Set SomeResource
loadSet1 = S.fromList [A 3, A 13, B "ack", B "adlfajdflkj"]

loadSet2 :: Set SomeResource
loadSet2 = S.fromList [B "ack", A 15]

testLoad :: IO ()
testLoad = do
  let config = LoadUnloadCallbacks simpleDeps simpleLoad simpleUnload simpleError
  newResources <- syncNewResources config noResources loadSet1
  print $ M.map value (resourcesMap newResources)
  newResources2 <- syncNewResources config newResources loadSet2
  print $ M.map value (resourcesMap newResources2)

asyncTestLoad ::IO ()
asyncTestLoad = do
  al <- startAsyncLoader asyncLoaderConfig ()
  newRequest al loadSet1
  load1 <- waitForResourceProcessing al (S.empty, loadSet1)
  print load1
  newRequest al loadSet2
  load2 <- waitForResourceProcessing al (loadSet1, loadSet2)
  print load2
  shutdownAsyncLoader al
  