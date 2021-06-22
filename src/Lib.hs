{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Lib
  ( ResourceLoaderConfig(..)
  , LoadedResources(..)
  , noLoadedResources
  , asyncLoad
  , fullLoad
  , asyncUnload
  , fullUnload
  , fullUnloadAsync
  , resourceCount
  , ResourceInfo(..)
  , someFunc
  ) where


import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async.Pool

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Data.Foldable
import Data.Tree
import Data.Row
import Data.Row.Variants (view)
import Data.Maybe (mapMaybe)

type ResourceRow = ("a" .== Int) .+ ("b" .== String)
type SomeResource a = Var a


--res :: SomeResource ResourceRow
--res = IsJust #a 3

data ResourceInfo l r = ResourceInfo {
    loadKey :: l,
    value :: r,
    dependsOn :: S.Set l,
    dependedBy :: S.Set l
  }
  deriving (Eq, Show)

-- | update a 'ResourceInfo' so that it depends on the resource named in param 1
addDependsOn :: (Ord l) => l -> ResourceInfo l r -> ResourceInfo l r
addDependsOn dep (ResourceInfo l r dOn dBy) = ResourceInfo l r (S.insert dep dOn) dBy

removeDependsOn :: (Ord l) => l -> ResourceInfo l r -> ResourceInfo l r
removeDependsOn dep (ResourceInfo l r dOn dBy) = ResourceInfo l r (S.delete dep dOn) dBy

addDependedBy :: (Ord l) => l -> ResourceInfo l r -> ResourceInfo l r
addDependedBy parent (ResourceInfo l r dOn dBy) = ResourceInfo l r dOn (S.insert parent dBy)

removeDependedBy :: (Ord l) => l -> ResourceInfo l r -> ResourceInfo l r
removeDependedBy parent (ResourceInfo l r dOn dBy) = ResourceInfo l r dOn (S.delete parent dBy)




type Loaded l r = ("loaded" .== r)
type NeedsLoad l r = ("needsLoad" .== (l,[l]) )
type Loading l r = ("loading" .== (Async r))

type EitherLoaded l r = Var ((Loaded l r) .+ (NeedsLoad l r))
type EitherLoading l r = Var ((Loaded l r) .+ (Loading l r))

data LoadedResources l r = LoadedResources {
  -- holds all currently loaded resources, indexed by the key used to load them
  resourceMap :: (M.Map l (ResourceInfo l r)),
  -- | Top level resources are those manually loaded by the outside caller, and do not get automatically unloaded when
  --   nothing depends on them.
  topLevelResources :: S.Set l
  }
  deriving (Eq, Show)

noLoadedResources :: LoadedResources l r
noLoadedResources = LoadedResources M.empty S.empty

lookupResource :: (Ord l) => l -> LoadedResources l r -> Maybe (ResourceInfo l r)
lookupResource k loaded = M.lookup k (resourceMap loaded)

resourceCount :: LoadedResources l r -> Int
resourceCount loaded = M.size (resourceMap loaded)

newtype LoadingMap l r = LoadingMap (M.Map l (ResourceInfo l r))



-- | Add values to indicate that resource 1 depends on resource 2
addDependency :: (Ord l) => l -> l -> LoadedResources l r -> LoadedResources l r
addDependency parent dependency loaded =
  loaded {
    resourceMap =
      M.adjust (addDependsOn dependency) parent $
        M.adjust (addDependedBy parent) dependency $
          (resourceMap loaded)
  }

-- | Removes values to show that resource 1 depends on resource 2
removeDependency :: (Ord l) => l -> l -> LoadedResources l r -> LoadedResources l r
removeDependency parent dependency loaded =
  loaded {
    resourceMap =
      M.adjust (removeDependsOn dependency) parent $
        M.adjust (removeDependedBy parent) dependency $
          (resourceMap loaded)
  }

-- | Add resource to the given resource map, and update the dependency lists of the dependent resources appropriately
addResource :: (Ord l) => ResourceInfo l r -> LoadedResources l r -> LoadedResources l r
addResource res loaded =
  let 
      k = loadKey res
      depOn = dependsOn res
      addResourceRecord rMap = rMap { resourceMap = M.insert k res (resourceMap rMap) }
      hookupDeps rMap = foldr (addDependency k) rMap depOn
  in 
    (hookupDeps . addResourceRecord) loaded


-- | Remove this resource from the given resource map, and unhook links to dependent resources
removeResource :: (Ord l) => ResourceInfo l r -> LoadedResources l r -> LoadedResources l r
removeResource res loaded =
    let k = loadKey res
        resDeps = dependsOn res
        deleteResourceRecord rMap = rMap { resourceMap = M.delete k (resourceMap rMap) }
        unhookDeps rMap = foldr (removeDependency k) rMap resDeps
    in
      (unhookDeps . deleteResourceRecord) loaded      

--
-- unloading
--


isUnloadable :: (Ord l) => l -> LoadedResources l r -> UnloadingMap l r -> Bool
isUnloadable unloadKey loaded uMap =
  case lookupResource unloadKey loaded of
    Nothing -> False
    Just res -> let dBy = (dependedBy res)
                    unloadingSet = S.fromList (M.keys uMap)
                in
                  -- To unload we need all of the dependedBy resources being unloaded AND
                  -- this resource is not 'pinned' as a top level resource.
                  S.isSubsetOf dBy unloadingSet && not (S.member unloadKey (topLevelResources loaded))

-- | When we remove a resource from the resource map, there may be some resources
--   that no longer have any "parent" resources that depend on them and can be unloaded.
--   Given a resource that was just removed, this looks for dependent resources with zero
--   dependedBy entries and returns a set containing keys of all these unloadable resources
findUnloadable :: (Ord l) => ResourceInfo l r -> LoadedResources l r -> UnloadingMap l r -> S.Set l
findUnloadable res loaded uMap =
  let checkResource k = if (isUnloadable k loaded uMap)
                        then Just k
                        else Nothing
  in S.fromList $ mapMaybe checkResource (S.toList $ dependsOn res)


data ResourceLoaderConfig l r = ResourceLoaderConfig
  {
    loadIO :: l -> [r] -> IO r,
    unloadIO :: l -> r -> IO (),
    dependenciesIO :: l -> IO [l]
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



--
-- loading
--


waitForLoad :: EitherLoading l r -> IO r
waitForLoad (view #loaded -> Just r) = return r
waitForLoad (view #loading -> Just r) = wait r


-- | Given a resource to load (by key) generate a tree of the things that need to load.
-- each elements of the tree is either 'Loaded' if the thing was already loaded, or 'needsload' if
-- the resource needs to be loaded.
loadDepsTree :: (Ord l) => ResourceLoaderConfig l r -> LoadedResources l r -> l -> IO (Tree (EitherLoaded l r))
loadDepsTree config loaded loadKey = do
  case (M.lookup loadKey (resourceMap loaded)) of
    Just x -> return (Node (IsJust #loaded (value x)) [])
    Nothing -> do
      deps <- dependenciesIO config loadKey
      depsTrees <- mapM (loadDepsTree config loaded) deps
      return (Node (IsJust #needsLoad (loadKey,deps)) depsTrees)

-- | Take the tree generated by 'loadDepsTree' and start loading resources. Each resource is loaded using
--   'async' and if a resource depends on other resources it must wait for those dependent resources to load
--   before starting its own load. Returns a monad that you run, passing the 'TaskGroup' to use and
--   a 'LoadingMap' with the stuff currently loaded. To chain together multiple 'asyncLoad' calls you can
-- grab the state result from and pass that into the next call.
asyncLoad :: forall l r.
  (Ord l, Show l) =>
  ResourceLoaderConfig l r -> Tree (EitherLoaded l r) -> Eff '[Reader TaskGroup, (State (LoadingMap l (EitherLoading l r))),IO] (EitherLoading l r)
asyncLoad _      !(Node (view #loaded -> Just i) vs) = return (IsJust #loaded i)
asyncLoad config !(Node (view #needsLoad -> Just (lKey,deps)) vs) = do
  (LoadingMap lMap) <- get @(LoadingMap l (EitherLoading l r))
  case (M.lookup lKey lMap) of
    -- already loading
    Just lr -> return (value lr)
    -- need to load
    Nothing -> do
      -- run 'asyncLoad' for all our dependencies, which will either find them already loading in the loadingMap or initiate
      -- an asynchronous load. Once this finishes every dep should have an 'EitherLoading l r'
      depsLoading <- traverse (asyncLoad config) vs
      taskGroup <- ask
      -- fork off a task to wait for dependencies and then load the resource once all dependencies are loaded
      res <- sendM $ async taskGroup $ do
        depsR <- mapM waitForLoad depsLoading   -- waits for all dependencies
        --putStrLn $ "start load of " ++ show lKey
        (loadIO config) lKey depsR
      -- now we have an async task that is loading this resource. Add it to the loading map so that
      -- anything that depends on this resource can find it and wait for it to load.
      let resInfo = (ResourceInfo lKey (IsJust #loading res) (S.fromList deps) S.empty)
      modify @(LoadingMap l (EitherLoading l r)) (updateLoadingDeps lKey resInfo)
      return (IsJust #loading res)
  where
    updateLoadingDeps lKey resInfo (LoadingMap lMap) =
      let deps = dependsOn resInfo
          -- modify the given resourceinfo so that it's 'dependedBy' field has the given 'parentKey'
          addDependedBy parentKey k lMap = M.adjust (\ri -> ri { dependedBy = S.insert parentKey (dependedBy ri)}) k lMap
      in
        LoadingMap $
          M.insert lKey resInfo $   -- add the resource
            foldr' (addDependedBy lKey) lMap deps   -- add 'resInfo' to the 'dependedBy' field of all deps


-- | Runs asynchronous loads to load all the resources and add them to the 'LoadedResources'. Once done,
--   returns the new 'LoadedResources' which now includes the resources that were just loaded.
fullLoad :: forall l r. (Ord l, Show l) =>
  TaskGroup -> ResourceLoaderConfig l r -> LoadedResources l r -> [l] -> IO (LoadedResources l r)
fullLoad tg config loaded loadList = do
  deps <- traverse (loadDepsTree config loaded) loadList
  -- A monad that, when run, does the actual loading.
  let loadAction = traverse (asyncLoad config) deps
  -- execute the actual loading. The initial state is an empty LoadingMap
  (LoadingMap s) <- runM $ execState (LoadingMap M.empty) $ runReader tg $ loadAction
  -- wait for all the loads to finish. We use the state 's' since it also has keys.
  s' <- M.traverseWithKey (\k x -> do v <- waitForLoad (value x); return $ x {value = v })  s
  -- add the resources in s' to the LoadedResources
  let loaded' = M.foldr (\r x -> addResource r x) loaded s'
  return $ loaded' {
    topLevelResources = S.union (topLevelResources loaded') (S.fromList loadList)
  } 
      
    



--
-- unloading
--

type NeedsUnload l r = ("needsUnload" .== (l,ResourceInfo l r))
type Unloading l r = ("unloading" .== Async ())
type EitherUnload l r = (Var (Unloading l r .+ NeedsUnload l r))
type UnloadingMap l r = M.Map l (EitherUnload l r)

dumpEitherUnload :: (Show l, Show r) =>  EitherUnload l r -> String
dumpEitherUnload (view #unloading -> Just asy) = "[Async]"
dumpEitherUnload (view #needsUnload -> Just (l,res)) = "[needsUnload " ++ show l ++ "," ++ show res ++ "]"

waitForUnload :: Var (Unloading l r) -> IO ()
waitForUnload (view #unloading -> Just a) = wait a

-- | Generate a set of things to unload. Finds the resource to unload, and also
--   finds other resources that are freed up because nothing depends on them.
--   Returns a monad you need to run, with the state containing a map of stuff to unload.
unloadDepsSet :: forall l r. (Ord l, Show l) =>
  ResourceLoaderConfig l r -> LoadedResources l r -> l -> Eff '[State (UnloadingMap l r)] ()
unloadDepsSet config loaded unloadKey =
  case lookupResource unloadKey loaded of
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
          let moreUnload = S.toList $ findUnloadable res loaded unloadingMap'
          --sendM $ putStrLn $ show unloadKey ++ " unload chain: " ++ show moreUnload
          put unloadingMap'
          mapM_ (unloadDepsSet config loaded) moreUnload
          return ()

-- | Given a map of things to unload (probably generated by 'unloadDepsSet') this will initialize the unload process, attempting
--   to multithread the unloading using 'async'. Returns a monad you need to run, passing in a 'TaskGroup' and the 'UnloadingMap'
--   Returns a tree of resources that are unloading for this one resource. Each 'Async' element of the tree is waiting on the
--   the 'Async' children so you really only need to wait on the root 'Async'.
--   You can bind multiple monads generated by 'asyncUnload', which allows you to unload multiple resources.
asyncUnload :: forall l r effs. (Ord l, Show l) =>
  ResourceLoaderConfig l r -> l -> Eff '[Reader TaskGroup, (State (UnloadingMap l r)),IO] (Tree (Var (Unloading l r)))
asyncUnload config unloadKey = do
  taskGroup <- ask
  unloadingMap <- get @(UnloadingMap l r)
  case (M.lookup unloadKey unloadingMap) of
    -- already unloading, just return it
    Just x -> case x of
                (view #unloading -> Just r) -> return (Node (IsJust #unloading r) [])
                (view #needsUnload -> Just (ul,res)) -> runUnload ul res taskGroup
    -- the unloadingmap should contain all the elements to be unloaded, so if we don't find one it's an error
    Nothing -> do
      sendM $ putStrLn $ "needsUnload not found for " ++ show unloadKey
      error "attempt to unload resource not marked for unloading, typically this means you tried to unload a resource that still had other resources depending on it"
  where
    runUnload ul res taskGroup = do
          -- We need the 'Async' values of unloading resources that depend on this resource
          let depending = S.toList (dependedBy res)
          dependAsyncs <- mapM (asyncUnload config) depending
          unloadTask <- sendM $ async taskGroup $ do
            -- resources that depend on this resource may refer to things in this resource,
            -- so we need to wait for all those unload first before unloading this one
            let waitForRoot = (\(Node x _) -> waitForUnload x)
            mapM_ waitForRoot dependAsyncs
            (unloadIO config) ul (value res)
          -- Add this to the unloadingMap, so that dependent resources can wait for it
          modify @(UnloadingMap l r) (M.insert unloadKey (IsJust #unloading unloadTask))
          return (Node (IsJust #unloading unloadTask) dependAsyncs)

-- | Starts unloading the specified resources and waits for all unloads to complete
fullUnload :: forall l r. (Ord l, Show l, Show r) =>
  TaskGroup -> ResourceLoaderConfig l r -> LoadedResources l r -> [l] -> IO (LoadedResources l r)
fullUnload tg config loaded unloadList  =do
    (loaded',waits) <- fullUnloadAsync tg config loaded unloadList
    wait waits
    return loaded'
      

-- | Starts running asynchronous unloads to unload all the resources and remove/add them from the 'LoadedResources', then
--   immedidately returns. The new 'LoadedResources' is returned immediately before all the unloads are done,
--   since you can't access them anymore anyways.
--   Also returns an @Async ()@ which you can wait upon if you want to wait until all the resources are unloaded
fullUnloadAsync :: forall l r. (Ord l, Show l) =>
  TaskGroup -> ResourceLoaderConfig l r -> LoadedResources l r -> [l] -> IO (LoadedResources l r, Async ())
fullUnloadAsync tg config loaded unloadList = do
  -- remove the unloads from the set of toplevel resources
  let loaded' = loaded { topLevelResources = foldr' S.delete (topLevelResources loaded) unloadList }
  -- find out resources that should be unloaded, now that their toplevel attribute is removed
  let toUnload = filter (\k -> isUnloadable k loaded' M.empty) unloadList
  let unloads = run $ execState M.empty $ traverse (unloadDepsSet config loaded') toUnload
  --putStrLn $ "unloads: " ++ show (M.map dumpEitherUnload unloads)
  -- A monad that, when run, does the actual unloading.
  let unloadAction = traverse (asyncUnload config) (M.keys unloads)
  -- execute the actual unloading. The initial state is the LoadingMap full of 'needsload' entries
  (as,s) <- runM $ runState unloads $ runReader tg $ unloadAction
  -- remove unloaded resources from the LoadedResources
  let loaded'' = foldr' (\k m -> case lookupResource k m of
                                    Nothing -> m
                                    Just r -> removeResource r m) loaded' (M.keys unloads)
  -- wait for all the things to unload. Depending on thread-safety, oftentimes you don't need to wait.
  -- You can just go do something else while resources are unloading.
  waitAll <- async tg $ mapM_ (\(Node x _) -> waitForUnload x) as
  return (loaded'',waitAll)

-- current recommended call: '@loadTest testLoaderConfig ["blargh"]
loadTest :: ResourceLoaderConfig String (SomeResource ResourceRow) -> [String] -> IO [SomeResource ResourceRow]
loadTest config ls = do
  let loaded = LoadedResources (M.fromList [("argh",ResourceInfo "argh" (IsJust #a 3) S.empty S.empty)]) S.empty
  loaded' <- withTaskGroup 4 $ \tg -> fullLoad tg config loaded ls
  putStrLn $ show loaded'
  return $ M.elems $ M.map value (resourceMap loaded')


-- current recommended call: @unloadTest testLoaderConfig ["blargh"]
unloadTest :: (Ord l, l ~ String, r ~ SomeResource ResourceRow) => ResourceLoaderConfig l r -> [String] -> IO ()
unloadTest config uls = do
  -- instead of trying to write all the dependencies by hand I'll just generate the load map from a list of ResourceInfo
  -- NOTE: order is important. A resource must appear in the list after all the resources it depends upon
  let listOfResources = [ (ResourceInfo "argh"(IsJust #a 1) S.empty S.empty)
                        , (ResourceInfo "ack" (IsJust #a 2) (S.singleton "argh") S.empty)
                        , (ResourceInfo "blargh"(IsJust #a 3) (S.fromList ["argh","ack"]) S.empty)
                        -- add a second thing that depends on argh so it won't get unloaded when you unload only "blargh"
                        , (ResourceInfo "alsodeponargh" (IsJust #a 4) (S.singleton "argh") S.empty)
                        ]
  let loaded = foldl (\m r -> addResource r m) (LoadedResources M.empty (S.fromList ["blargh","ack"])) listOfResources
  loaded' <- withTaskGroup 4 $ \tg -> fullUnload tg config loaded uls
  putStrLn $ show loaded'


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
