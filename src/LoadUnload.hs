{-|
Module      : LoadUnload
Description : Functions to asynchronously load and unload resources and their dependencies.
Copyright   : (c) Gary Haussmann 2021
Stability   : experimental

The code in here computes resource dependencies and loads the resources via IO. Loading is
done asynchronously using task groups so that you can control the amount of tasks to use for loading/unloading.

Functions exist to load resources and to unload resources, but not both. The ordering and combining of loads and unloads is done
at a higher level by composing functions from this module.

The functions take a @LoadedResources l r@ representing resources already loaded, and then after performing their operations
return a new @LoadedResources l r@ as a result.

You load a resource by specifying it's key @l@ which could be a file path or a unique identifier. After loading the resource
will appear in @LoadedResources l r@ as a 'ResourceInfo' data structure value which you get by looking up the key @l@

You need to provide a configuration data structure 'ResourceLoaderConfig' which will:
- compute the dependencies for any given resource key,
- load a resource given a key and the dependency values, 
- unload a resource given a key and the resource value

The main functions are 'fullLoad' and 'fullUnload' which will queue up tasks to load/unload resources in the correct order (so that
dependencies are loaded before things that depend on them, and vice-versa for unloading).
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module LoadUnload (
    ResourceLoaderConfig(..)
  , LoadedResources(..)
  , noLoadedResources
  , asyncLoad
  , fullLoad
  , asyncUnload
  , fullUnload
  , fullUnloadAsync
  , resourceCount
  , syncLoad
  , ResourceInfo(..)
) where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader

import Control.Concurrent.Async.Pool

import qualified Data.Set as S   -- Set is strict already
import qualified Data.Map.Strict as M  -- need the strict version so eval happens during loading

import Data.Foldable
import Data.Tree
import Data.Row
import Data.Row.Variants (view)
import Data.Maybe (mapMaybe)


-- | Contains the resource itself as well as dependency data for proper load/unload ordering
data ResourceInfo l r = ResourceInfo {
    loadKey :: l,
    value :: r,
    dependsOn :: (S.Set l),
    dependedBy :: (S.Set l)
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

-- | The data structure usually passed around by 'fullLoad' and 'fullUnload'. They will take a 'LoadedResources' and
--   return a new 'LoadedResources'. The type @l@ is the key used to find the resource (usually a file path as string)
--   and @r@ is the data type of resources after they are loaded. @r@ is usually a sum type of the different
--   kinds of resources.
--
--   Data members are strict so all this stuff is evaluated during the asynchronous loading process. Without the
--   bang patterns some data may get thunked and not evaluated until later by some other task, well after it was supposed
--   to have been laoded.
data LoadedResources l r = LoadedResources {
  -- | holds all currently loaded resources, indexed by the key used to load them
  resourceMap :: !(M.Map l (ResourceInfo l r)),
  -- | Top level resources are those manually loaded by the outside caller. This is in contrast with non-toplevel
  --   resources which were not requested manually but are loaded because other loaded resources depend on them.
  topLevelResources :: !(S.Set l)
  }
  deriving (Eq, Show)

-- | An initial state for 'LoadedResources'
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

-- | Is the resource specified unloadable? All resources depending on it must be unloading
--   AND it must not be manually requested by the user (a toplevel resource)
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

--
-- loading
--


-- | Waits for a load to finish, or return immediately if it was already loaded.
waitForLoad :: EitherLoading l r -> IO r
waitForLoad elr = switch elr $
       #loaded .== (\r -> return r)
    .+ #loading .== (\r -> wait r)


-- | Given a resource to load (by key) generate a tree of the things that need to load.
-- each elements of the tree is either 'Loaded' if the thing was already loaded, or 'needsload' if
-- the resource needs to be loaded.
loadDepsTree :: (Ord l) => ResourceLoaderConfig l r -> LoadedResources l r -> l -> IO (Tree (EitherLoaded l r))
loadDepsTree config loaded lKey = do
  case (M.lookup lKey (resourceMap loaded)) of
    Just x -> return (Node (IsJust #loaded (value x)) [])
    Nothing -> do
      deps <- dependenciesIO config lKey
      depsTrees <- mapM (loadDepsTree config loaded) deps
      return (Node (IsJust #needsLoad (lKey,deps)) depsTrees)

-- | Take the tree generated by 'loadDepsTree' and start loading resources. Each resource is loaded using
--   'async' and if a resource depends on other resources it must wait for those dependent resources to load
--   before starting its own load. Returns a monad that you run, passing the 'TaskGroup' to use and
--   a 'LoadingMap' with the stuff currently loaded. To chain together multiple 'asyncLoad' calls you can
-- grab the state result from and pass that into the next call.
asyncLoad :: forall l r.
  (Ord l, Show l) =>
  ResourceLoaderConfig l r -> Tree (EitherLoaded l r) -> Eff '[Reader TaskGroup, (State (LoadingMap l (EitherLoading l r))),IO] (EitherLoading l r)
asyncLoad _      !(Node (IsJust (Label :: Label "loaded") i) _) = return (IsJust #loaded i)
asyncLoad config !(Node (IsJust (Label :: Label "needsLoad") (lKey,deps)) vs) = do
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
      let resInfo = (ResourceInfo lKey (IsJust #loading res) (S.fromList deps) S.empty) :: ResourceInfo l (EitherLoading l r)
      modify @(LoadingMap l (EitherLoading l r)) (updateLoadingDeps lKey resInfo)
      return (IsJust #loading res)
  where
    updateLoadingDeps key resInfo (LoadingMap lMap) =
      let depOn = dependsOn resInfo
          -- modify the given resourceinfo so that it's 'dependedBy' field has the given 'parentKey'
          addDepInMap parentKey k m = M.adjust (\ri -> ri { dependedBy = S.insert parentKey (dependedBy ri)}) k m
      in
        LoadingMap $
          M.insert key resInfo $   -- add the resource
            foldr' (addDepInMap lKey) lMap depOn   -- add 'resInfo' to the 'dependedBy' field of all deps
asyncLoad _ (Node _ _) = error "Pattern match fail in asyncLoad"


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
  s' <- M.traverseWithKey (\_ x -> do v <- waitForLoad (value x); return $ x {value = v })  s
  -- add the resources in s' to the LoadedResources
  let loaded' = M.foldr (\r x -> addResource r x) loaded s'
  return $ loaded' {
    topLevelResources = S.union (topLevelResources loaded') (S.fromList loadList)
  }
      
-- | Loads the resources in the tree starting with children and eventually loading the root node.
--   Returns a new 'LoadedResources' where the resource is loaded.
syncLoadTree :: forall l r.
  (Ord l, Show l) =>
  ResourceLoaderConfig l r -> LoadedResources l r -> Tree (EitherLoaded l r) -> IO (LoadedResources l r)
syncLoadTree _      loaded !(Node (IsJust (Label :: Label "loaded") i) _) = return loaded
syncLoadTree config loaded !(Node (IsJust (Label :: Label "needsLoad") (lKey,deps)) vs) = do
  case (M.lookup lKey (resourceMap loaded)) of
    -- already loaded
    Just _ -> return loaded
    Nothing -> do
      putStrLn $ "start load of " ++ show lKey
      -- load all deps
      loadedWithDeps <- foldlM (syncLoadTree config) loaded vs
      let maybeDepValues = traverse (\l -> (M.lookup l (resourceMap loadedWithDeps))) deps
      case (maybeDepValues) of
        Nothing -> error "bad loading deps in syncLoad"
        Just depValues -> do
          r <- (loadIO config) lKey (fmap value depValues)
          -- now we have an async task that is loading this resource. Add it to the loading map so that
          -- anything that depends on this resource can find it and wait for it to load.
          let resInfo = (ResourceInfo lKey r (S.fromList deps) S.empty) :: ResourceInfo l r
          return $ addResource resInfo loadedWithDeps
syncLoadTree _ _ (Node _ _) = error "Pattern match fail in syncLoad"

syncLoad :: forall l r. (Ord l, Show l) => ResourceLoaderConfig l r -> LoadedResources l r -> [l] -> IO (LoadedResources l r)
syncLoad config loaded loadList = do
  -- find deps
  deps <- traverse (loadDepsTree config loaded) loadList
  -- recursive load of stuff
  loadedAll <- foldlM (syncLoadTree config) loaded deps
  return $ loadedAll {
    topLevelResources = S.union (topLevelResources loadedAll) (S.fromList loadList)
  }



--
-- unloading
--

type NeedsUnload l r = ("needsUnload" .== (l,ResourceInfo l r))
type Unloading l r = ("unloading" .== Async ())
type EitherUnload l r = (Var (Unloading l r .+ NeedsUnload l r))
type UnloadingMap l r = M.Map l (EitherUnload l r)

waitForUnload :: Var (Unloading l r) -> IO ()
waitForUnload (view #unloading -> Just a) = wait a
waitForUnload _ = error "Bad pattern match"

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
asyncUnload :: forall l r. (Ord l, Show l) =>
  ResourceLoaderConfig l r -> l -> Eff '[Reader TaskGroup, (State (UnloadingMap l r)),IO] (Tree (Var (Unloading l r)))
asyncUnload config unloadKey = do
  taskGroup <- ask
  unloadingMap <- get @(UnloadingMap l r)
  case (M.lookup unloadKey unloadingMap) of
    -- already unloading, just return it
    Just x -> case x of
                (view #unloading -> Just r) -> return (Node (IsJust #unloading r) [])
                (view #needsUnload -> Just (ul,res)) -> runUnload ul res taskGroup
                _ -> error "pattern match error"
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
fullUnload :: forall l r. (Ord l, Show l) => TaskGroup -> ResourceLoaderConfig l r -> LoadedResources l r -> [l] -> IO (LoadedResources l r)
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
  (as,_) <- runM $ runState unloads $ runReader tg $ unloadAction
  -- remove unloaded resources from the LoadedResources
  let loaded'' = foldr' (\k m -> case lookupResource k m of
                                    Nothing -> m
                                    Just r -> removeResource r m) loaded' (M.keys unloads)
  -- wait for all the things to unload. Depending on thread-safety, oftentimes you don't need to wait.
  -- You can just go do something else while resources are unloading.
  waitAll <- async tg $ mapM_ (\(Node x _) -> waitForUnload x) as
  return (loaded'',waitAll)
