{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Lib
  ( someFunc
  ) where

import qualified GHC.Generics as GHC
import Generics.SOP

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async.Pool

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Functor.Foldable
import Data.Functor.Base (TreeF(..))

import Data.Foldable
import Data.Distributive
import Data.Tree
import Data.Row
import Data.Row.Variants (view)
import Data.Maybe (mapMaybe, maybe)

import Zorja.Patchable
import Zorja.Collections.PatchableSet
import qualified Zorja.Collections.MapValDelta as MVD

type ResourceRow = ("a" .== Int) .+ ("b" .== String)
type SomeResource a = Var a


res :: SomeResource ResourceRow
res = IsJust #a 3

data ResourceInfo l r = ResourceInfo {
    value :: r,
    dependsOn :: S.Set l,
    dependedBy :: S.Set l
  }
  deriving (Eq, Show)

-- | update a 'ResourceInfo' so that it depends on the resource named in param 1
addDependsOn :: (Ord l) => l -> ResourceInfo l r -> ResourceInfo l r
addDependsOn dep (ResourceInfo r dOn dBy) = ResourceInfo r (S.insert dep dOn) dBy

removeDependsOn :: (Ord l) => l -> ResourceInfo l r -> ResourceInfo l r
removeDependsOn dep (ResourceInfo r dOn dBy) = ResourceInfo r (S.delete dep dOn) dBy

addDependedBy :: (Ord l) => l -> ResourceInfo l r -> ResourceInfo l r
addDependedBy parent (ResourceInfo r dOn dBy) = ResourceInfo r dOn (S.insert parent dBy)

removeDependedBy :: (Ord l) => l -> ResourceInfo l r -> ResourceInfo l r
removeDependedBy parent (ResourceInfo r dOn dBy) = ResourceInfo r dOn (S.delete parent dBy)



data LoaderState l r = LoaderState
  {
    loadedResources :: LoadedMap l r,
    topLevelResource :: l,
    errorMessages :: [String]
  }
  deriving (Eq, Show)


-- | Add values to indicate that resource 1 depends on resource 2
addDependency :: (Ord l) => l -> l -> LoadedMap l r -> LoadedMap l r
addDependency parent dependency rMap =
      M.adjust (addDependsOn dependency) parent $
        M.adjust (addDependedBy parent) dependency $
          rMap

removeDependency :: (Ord l) => l -> l -> LoadedMap l r -> LoadedMap l r
removeDependency parent dependency rMap =
      M.adjust (removeDependsOn dependency) parent $
        M.adjust (removeDependedBy parent) dependency $
          rMap

-- | Add resource to the given resource map, and update the dependency lists of the dependent resources appropriately
addResource :: (Ord l) => l -> ResourceInfo l r -> LoadedMap l r -> LoadedMap l r
addResource k res loaded =
  let resDeps = (dependsOn res)
      addResourceRecord rMap = M.insert k res rMap
      hookupDeps rMap = foldr (addDependency k) rMap resDeps
  in 
    (hookupDeps . addResourceRecord) loaded

-- | Remove this resource from the given resource map, and unhook links to dependent resources
removeResource :: (Ord l) => l -> ResourceInfo l r -> LoadedMap l r -> LoadedMap l r
removeResource k res loaded =
    let resDeps = (dependsOn res)
        deleteResourceRecord rMap = M.delete k rMap
        unhookDeps rMap = foldr (removeDependency k) rMap resDeps
    in
      (unhookDeps . deleteResourceRecord) loaded      

isUnloadable :: (Ord l) => ResourceInfo l r -> UnloadingMap l r -> Bool
isUnloadable res uMap =
  let dBy = (dependedBy res)
      unloadingSet = S.fromList (M.keys uMap)
  in
    -- are all of the dependedBy resources being unloaded?
    S.isSubsetOf dBy unloadingSet

-- | When we remove a resource from the resource map, there may be some resources
--   that no longer have any "parent" resources that depend on them and can be unloaded.
--   Given a resource that was just removed, this looks for dependent resources with zero
--   dependedBy entries and returns a set containing keys of all these unloadable resources
findUnloadable :: (Ord l) => ResourceInfo l r -> LoadedMap l r -> UnloadingMap l r -> S.Set l
findUnloadable res loadedMap uMap =
  let checkResource k = case M.lookup k loadedMap of
                          Nothing -> Nothing 
                          Just res -> if (isUnloadable res uMap)
                                      then Just k
                                      else Nothing
  in S.fromList $ mapMaybe checkResource (S.toList $ dependsOn res)




type ResourceLoad l r = l -> [r] -> IO r
type ResourceUnload l r = l -> r -> IO ()
type ResourceDependencies l r = l -> IO [l]

data ResourceLoaderConfig l r = ResourceLoaderConfig
  {
    loadIO :: ResourceLoad l r,
    unloadIO :: ResourceUnload l r,
    dependenciesIO :: ResourceDependencies l r
  }

testLoaderConfig :: ResourceLoaderConfig String (SomeResource ResourceRow)
testLoaderConfig = 
  ResourceLoaderConfig
    {
      loadIO = \l m -> do
        threadDelay (30000 * length l)
        putStrLn ("loaded " ++ l)
        return (IsJust #b l)
    , unloadIO = \l r -> do threadDelay 60000; putStrLn ("delete " ++ l); return ()
    , dependenciesIO = \l -> do threadDelay 40000;
                                putStrLn ("depends " ++ l);
                                return $ if (l == "argh")
                                         then ["ack"]
                                         else if (l == "blargh")
                                         then ["ack","argh","forsooth"]
                                         else if (l == "ack")
                                         then ["elephants", "watermelon"]
                                         else []
    }


type Loaded l r = ("loaded" .== r)
type NeedsLoad l r = ("needsLoad" .== (l,[l]) )
type Loading l r = ("loading" .== (Async r))

type EitherLoaded l r = Var ((Loaded l r) .+ (NeedsLoad l r))
type EitherLoading l r = Var ((Loaded l r) .+ (Loading l r))

type LoadedMap l r = M.Map l (ResourceInfo l r)
type LoadingMap l r = (M.Map l (ResourceInfo l r))


waitForLoad :: EitherLoading l r -> IO r
waitForLoad (view #loaded -> Just r) = return r
waitForLoad (view #loading -> Just r) = do putStrLn "waiting..."; wait r


-- | Given a resource to load (by key) generate a tree of the things that need to load.
-- each elements of the tree is either 'Loaded' if the thing was already loaded, or 'needsload' if
-- the resource needs to be loaded.
loadDepsTree :: (Ord l) => LoadedMap l r -> ResourceLoaderConfig l r -> l -> IO (Tree (EitherLoaded l r))
loadDepsTree loaded config loadKey = do
  case (M.lookup loadKey loaded) of
    Just x -> return (Node (IsJust #loaded (value x)) [])
    Nothing -> do
      deps <- dependenciesIO config loadKey
      depsTrees <- mapM (loadDepsTree loaded config) deps
      return (Node (IsJust #needsLoad (loadKey,deps)) depsTrees)

-- | Take the tree generated by 'loadDepsTree' and start loading resources. Each resource is loaded using
--   'async' and if a resource depends on other resources it must wait for those dependent resources to load
--   before starting its own load. Returns a monad that you run, passing the 'TaskGroup' to use and
--   a 'LoadingMap' with the stuff currently loaded. To chain together multiple 'asyncLoad' calls you can
-- grab the state result from and pass that into the next call.
asyncLoad :: forall l r effs.
  (Ord l, Show l) =>
  ResourceLoaderConfig l r -> Tree (EitherLoaded l r) -> Eff '[Reader TaskGroup, (State (LoadingMap l (EitherLoading l r))),IO] (EitherLoading l r)
asyncLoad _      (Node (view #loaded -> Just i) vs) = return (IsJust #loaded i)
asyncLoad config (Node (view #needsLoad -> Just (lKey,deps)) vs) = do
  loadingMap <- get @(LoadingMap l (EitherLoading l r))
  case (M.lookup lKey loadingMap) of
    Just lr -> return (value lr)
    Nothing -> do
      -- run 'asyncLoad' for all our dependencies, which will either find them already loading in the loadingMap or initiate
      -- an asynchronous load. Once this finishes every dep should have an 'EitherLoading l r'
      depsLoading <- traverse (asyncLoad config) vs
      taskGroup <- ask
      -- fork off a task to wait for dependencies and then load the resource once all dependencies are loaded
      res <- sendM $ async taskGroup $ do
        depsR <- mapM waitForLoad depsLoading   -- waits for all dependencies
        putStrLn $ "start load of " ++ show lKey
        (loadIO config) lKey depsR
      -- now we have an async task that is loading this resource. Add it to the loading map so that
      -- anything that depends on this resource can find it and wait for it to load.
      let resInfo = (ResourceInfo (IsJust #loading res) (S.fromList deps) S.empty)
      modify @(LoadingMap l (EitherLoading l r)) (updateLoadingDeps lKey resInfo)
      return (IsJust #loading res)
  where
    updateLoadingDeps lKey resInfo loadingMap =
      let deps = dependsOn resInfo
          addDependedBy parentKey k lMap = M.adjust (\ri -> ri { dependedBy = S.insert parentKey (dependedBy ri)}) k lMap
      in
        M.insert lKey resInfo $ foldr (addDependedBy lKey) loadingMap deps
        

type NeedsUnload l r = ("needsUnload" .== (l,ResourceInfo l r))
type Unloading l r = ("unloading" .== Async ())
type EitherUnload l r = (Var (Unloading l r .+ NeedsUnload l r))
type UnloadingMap l r = M.Map l (EitherUnload l r)

dumpEitherUnload :: (Show l, Show r) =>  EitherUnload l r -> String
dumpEitherUnload (view #unloading -> Just asy) = "[Async]"
dumpEitherUnload (view #needsUnload -> Just (l,res)) = "[needsUnload " ++ show l ++ "," ++ show res ++ "]"

-- | Generate a set of things to unload. Finds the resource to unload, and also
--   finds other resources that are freed up because nothing depends on them.
--   Returns a monad you need to run, with the state containing a map of stuff to unload.
unloadDepsSet :: forall l r. (Ord l, Show l) => ResourceLoaderConfig l r -> LoadedMap l r -> l -> Eff '[(State (UnloadingMap l r)), IO] ()
unloadDepsSet config loadedMap unloadKey =
  case M.lookup unloadKey loadedMap of
    Nothing -> return ()
    Just res -> do
      unloadingMap <- get @(UnloadingMap l r)
      case M.lookup unloadKey unloadingMap of
        -- skip if it's already unloading
        Just _ -> return ()
        Nothing -> do
          -- add ourselves to the unloading map
          let unloadingMap' = (M.insert unloadKey (IsJust #needsUnload (unloadKey, res)) unloadingMap)
          -- look for dependencies that can be unloaded and unload them as well
          let moreUnload = S.toList $ findUnloadable res loadedMap unloadingMap'
          sendM $ putStrLn $ show unloadKey ++ " unload chain: " ++ show moreUnload
          put unloadingMap'
          mapM_ (unloadDepsSet config loadedMap) moreUnload
          return ()

-- | Given a map of things to unload that was generated by 'unloadDepsSet' this will initialize the unload process, attempting
--   to multithread the unloading using 'async'. Returns a monad you need to run, passing in a 'TaskGroup' and the 'UnloadingMap'
--   Returns a tree of resources that are unloading.
asyncUnload :: forall l r. (Ord l, Show l) =>
  ResourceLoaderConfig l r -> l -> Eff '[Reader TaskGroup, (State (UnloadingMap l r)), IO] (Tree (Var (Unloading l r)))
asyncUnload config unloadKey = do
  taskGroup <- ask
  unloadingMap <- get @(UnloadingMap l r)
  case (M.lookup unloadKey unloadingMap) of
    -- already unloading, just return it
    Just x -> case x of
                (view #unloading -> Just r) -> return (Node (IsJust #unloading r) [])
                (view #needsUnload -> Just (ul,res)) -> runUnload ul res taskGroup
    Nothing -> do
      sendM $ putStrLn $ "needsUnload not found for " ++ show unloadKey
      error "attempt to unload resource not marked for unloading"
  where
    runUnload ul res taskGroup = do
          -- We need the 'Async' values of unloading resources that depend on this resource
          let depending = S.toList (dependedBy res)
          dependAsyncs <- mapM (asyncUnload config) depending
          unloadTask <- sendM $ async taskGroup $ do
            -- resources that depend on this resource may refer to things in this resource,
            -- so we need to wait for all those unload first before unloading this one
            let waitForRoot = (\(Node (view #unloading -> Just asy) _) -> wait asy)
            mapM_ waitForRoot dependAsyncs
            (unloadIO config) ul (value res)
          -- Add this to the unloadingMap, so that dependent resources can wait for it
          modify @(UnloadingMap l r) (M.insert unloadKey (IsJust #unloading unloadTask))
          return (Node (IsJust #unloading unloadTask) dependAsyncs)


loadTest :: (Ord l, l ~ String, r ~ SomeResource ResourceRow) => ResourceLoaderConfig l r -> String -> IO r
loadTest config l = do
  let loadedMap = M.fromList [("argh",ResourceInfo (IsJust #a 3) S.empty S.empty)]
  deps <- loadDepsTree loadedMap config l
  let loadingMap = M.empty
  withTaskGroup 4 $ \tg -> do
    (a,s) <- runM $ runState loadingMap $ runReader tg $ asyncLoad config deps
    result <- waitForLoad a
    s' <- traverse (\x -> do v <- waitForLoad (value x); return x {value = v}) s
    putStrLn $ show s'
    return result


unloadTest :: (Ord l, l ~ String, r ~ SomeResource ResourceRow) => ResourceLoaderConfig l r -> String -> IO ()
unloadTest config ul = do
  -- instead of trying to write all the dependencies by hand I'll just make a list of ResourceInfo and
  -- add resources from a list so that the dependencies are set up correctly
  -- NOTE: order is important. A resource must appear in the list after all the resources it depends upon
  let listOfResources = [ ("argh",ResourceInfo (IsJust #a 1) S.empty S.empty)
                        , ("ack", ResourceInfo (IsJust #a 2) (S.singleton "argh") S.empty)
                        , ("blargh", ResourceInfo (IsJust #a 3) (S.fromList ["argh","ack"]) S.empty)
                        ]
  let loadedMap = foldl (\m (l,r) -> addResource l r m) M.empty listOfResources
  withTaskGroup 4 $ \tg -> do
    ((),unloads) <- runM $ runState M.empty $ unloadDepsSet config loadedMap ul
    putStrLn $ "unloads: " ++ show (M.map dumpEitherUnload unloads)
    (au,s) <- runM $ runState unloads $ runReader tg $ traverse (asyncUnload config) (M.keys unloads)
    traverse waitForUnload au
    putStrLn "Unload done"
    return ()
  where
    waitForUnload (Node (view #unloading -> Just a) _) = do wait a; return ()
    waitForUnload _                                    = return ()


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
