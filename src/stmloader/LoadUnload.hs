{-|
Module      : LoadUnload
Description : Functions to asynchronously load and unload resources and their dependencies.
Copyright   : (c) Gary Haussmann 2021
Stability   : experimental

The code in here computes resource dependencies and loads the resources via IO. Loading is
done asynchronously using task groups so that you can control the amount of tasks to use for loading/unloading.

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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module STMLoader.LoadUnload where

import Control.Exception
import Control.Monad.Freer
import Control.Monad.Freer.State

-- Data.Set and Data.Map have a lot of overlapping functions so we only directly expose
-- the data types
import Data.Set (Set)
import qualified Data.Set as S   -- Set is strict already
import Data.Map (Map)
import qualified Data.Map.Strict as M  -- need the strict version so eval happens during loading
import Data.Foldable
import Data.Maybe (isJust)


-- | Contains the resource itself as well as dependency data for proper load/unload ordering
data ResourceInfo l r = ResourceInfo {
    loadKey :: l,
    value :: r,
    dependsOn :: Set l
  }
  deriving (Eq, Show)

data LoadUnloadError l r e =
     DepsFailed l e
  |  LoadFailed l e
  |  UnloadFailed l r e

data TriageError e =
    Drop
  | Retry
  | Report e

data LoadUnloadConfig l r e =
  LoadUnloadConfig
  {
    findDependencies :: l -> IO [l],
    loadResource :: l -> IO (ResourceInfo l r),
    unloadResource :: ResourceInfo l r -> IO (),
    -- | When a load, deps, or unload fails, 
    processException :: forall err. (Exception err) => LoadUnloadError l r err -> TriageError e
  }


-- | The data structure usually passed around by 'fullLoad' and 'fullUnload'. They will take a 'LoadedResources' and
--   return a new 'LoadedResources'. The type @l@ is the key used to find the resource (usually a file path as string)
--   and @r@ is the data type of resources after they are loaded. @r@ is usually a sum type of the different
--   kinds of resources.
--
--   Data members are strict so all this stuff is evaluated during the asynchronous loading process. Without the
--   bang patterns some data may get thunked and not evaluated until later by some other task, well after it was supposed
--   to have been laoded.
data ResourcesMap l r = ResourcesMap {
  -- | holds all currently loaded resources, indexed by the key used to load them
  resourcesMap :: !(Map l (ResourceInfo l r)),
  -- | Top level resources are those manually loaded by the outside caller. This is in contrast with non-toplevel
  --   resources which were not requested manually but are loaded because other loaded resources depend on them.
  topLevelResources :: !(Set l)
  }
  deriving (Eq, Show)

-- | An initial state for 'LoadedResources'
noResources :: ResourcesMap l r
noResources = ResourcesMap M.empty S.empty

lookupResource :: (Ord l) => l -> ResourcesMap l r -> Maybe (ResourceInfo l r)
lookupResource k loaded = M.lookup k (resourcesMap loaded)

addResource :: (Ord l) => ResourceInfo l r -> ResourcesMap l r -> ResourcesMap l r
addResource res (ResourcesMap rm top) = ResourcesMap (M.insert (loadKey res) res rm) top

removeResource :: (Ord l) => ResourceInfo l r -> ResourcesMap l r -> ResourcesMap l r
removeResource (ResourceInfo k _ _) = removeResourceByKey k

removeResourceByKey :: (Ord l) => l -> ResourcesMap l r -> ResourcesMap l r
removeResourceByKey k (ResourcesMap rm top) = ResourcesMap (M.delete k rm) top

resourceCount :: ResourcesMap l r -> Int
resourceCount loaded = M.size (resourcesMap loaded)

newtype LoadingMap l r = LoadingMap (M.Map l (ResourceInfo l r))




data WalkState l = WalkState (Set l) (Map l (ResourceInfo l ()))
  deriving (Eq, Show)

data WalkMonad l x where
  GetW :: WalkMonad l (WalkState l)
  PutW :: WalkState l -> WalkMonad l ()

getW :: forall l effs. (Member (WalkMonad l) effs) => Eff effs (WalkState l)
getW = send GetW

putW :: (Member (WalkMonad l) effs) => WalkState l -> Eff effs ()
putW = send . PutW

runWalkState :: forall l effs. WalkState l -> Eff (WalkMonad l ': effs) ~> Eff effs
runWalkState ws = evalState ws . walkToState
  where
    walkToState :: Eff (WalkMonad l ': effs) ~> Eff (State (WalkState l) ': effs)
    walkToState = reinterpret $ \case
      GetW -> get
      PutW w -> put w

-- | Given a 'ResourcesMap' and a new top-level set, works out which resources need to be loaded and the set of resources to be kept
walkResourceChanges :: forall l r e. (Ord l) => LoadUnloadConfig l r e -> ResourcesMap l r -> Set l -> IO (WalkState l)
walkResourceChanges config rm newTopLevel = 
  let findDeps = findDependencies config
      markRoots = S.toList newTopLevel
      rMap = resourcesMap rm
  in
    runM $ runWalkState @l (WalkState S.empty M.empty) $ do
      traverse_ (markForLoad @l @r rMap findDeps) markRoots
      getW @l

markForLoad :: forall l r effs. (Member (WalkMonad l) effs, LastMember IO effs, Ord l) 
  => Map l (ResourceInfo l r) -> (l -> IO [l]) -> l -> Eff effs ()
markForLoad loadedMap findDeps toMark = do
  --sendM $ print toMark
  (WalkState marked _) <- getW @l
  -- if already in the marked set we don't need to check for loading or anything
  if S.member toMark marked
  then return ()
  else do
    -- we need to scan the deps, even if loaded, since we need to mark deps as well
    deps <- sendM $ findDeps toMark
    traverse_ (markForLoad @l loadedMap findDeps) deps
    (WalkState m l) <- getW @l
    let m' = S.insert toMark m
    -- make sure we're loading this resource if it isn't already loaded
    let l' = if isJust (M.lookup toMark loadedMap) || isJust (M.lookup toMark l)
             then l
             else M.insert toMark (ResourceInfo toMark () (S.fromList deps)) l
    putW (WalkState m' l')



-- | An intermediate state used when in the process of loading/unloading things. You wrap the 'ResourcesMap' with this
--   transient data structure and perform the load/unload actions, updating the intermediate state. When completed you
--   have a new 'ResourcesMap' and maybe some errors. Then you intake more load/unload requests and repeat the process.
data IntermediateResourceMap l r e =
  IntermediateResourceMap 
  {
    currentlyLoaded :: Map l (ResourceInfo l r),
    loadingSet :: Map l (ResourceInfo l ()),
    unloadingSet :: Map l (ResourceInfo l r),
    errorLog :: Map l String
  }

-- | Switch the given ResourcesMap to a new set of resources for topLevelResources, performing
--   the needed loads and unloads. Synchronous, so once this returns all the loads/unloads will have completed.
syncNewResources :: (Ord l) => LoadUnloadConfig l r e -> ResourcesMap l r -> Set l -> IO (ResourcesMap l r)
syncNewResources config rm newTop = do
  intermediateResult <- findLoadsUnloads config rm newTop 
                        >>= runLoads config 
                        >>= runUnloads config
  return $ ResourcesMap (currentlyLoaded intermediateResult) newTop


findLoadsUnloads :: (Ord l) => LoadUnloadConfig l r e-> ResourcesMap l r -> Set l -> IO (IntermediateResourceMap l r e)
findLoadsUnloads config rm@(ResourcesMap res _) newTop = do
  (WalkState marked toLoad) <- walkResourceChanges config rm newTop
  let toUnload = S.difference (S.fromList $ M.keys res) marked
  let shouldUnload = \ri -> S.member (loadKey ri) toUnload
  return $ IntermediateResourceMap res toLoad (M.filter shouldUnload res) M.empty

runLoads :: (Ord l) => LoadUnloadConfig l r e-> IntermediateResourceMap l r e -> IO (IntermediateResourceMap l r e)
runLoads config im@(IntermediateResourceMap rm loadz unloadz errorz) =
  case nextLoad loadz rm of
    Nothing -> return im
    Just k  -> do
      loadResult <- loadResource config k
      let loadz' = M.delete k loadz
      let rm' = M.insert (loadKey loadResult) loadResult rm
      runLoads config (IntermediateResourceMap rm' loadz' unloadz errorz)

runUnloads :: (Ord l) => LoadUnloadConfig l r e -> IntermediateResourceMap l r e -> IO (IntermediateResourceMap l r e)
runUnloads config im@(IntermediateResourceMap rm loadz unloadz errorz) = do
  case nextUnload unloadz rm of
    Nothing -> return im
    Just u -> do
      unloadResource config u
      let unloadz' = M.delete (loadKey u) unloadz
      let rm' = M.delete (loadKey u) rm
      runUnloads config (IntermediateResourceMap rm' loadz unloadz' errorz)


nextLoad :: (Ord l) => Map l (ResourceInfo l ()) -> Map l (ResourceInfo l r) -> Maybe l
nextLoad toLoad loaded = 
  let -- we can load a resource if all it's dependencies are loaded (in the 'loaded' map)
      canLoad (ResourceInfo _ _ deps) = all (\k -> M.member k loaded) deps
  in
    loadKey <$> find canLoad toLoad
                                    
nextUnload :: (Ord l) => Map l (ResourceInfo l r) -> Map l (ResourceInfo l r) -> Maybe (ResourceInfo l r)
nextUnload toUnload loaded =
  let doesNotDependOn k (ResourceInfo _ _ deps) = not $ S.member k deps
      canUnload (ResourceInfo k _ _) = all (doesNotDependOn k) loaded
  in
    find canUnload toUnload

