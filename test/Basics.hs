{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Basics (
   basicsTests
   ) where
  
import Control.Concurrent.Async.Pool
import Control.Monad.IO.Class

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Generators
import TestViz
import STMLoader.LoadUnload
import STMLoader.AsyncLoader

basicsTests :: IO Bool
basicsTests = checkSequential $$(discover)




-- | If you request to load a single resource the should be at least one resource loaded as a result
prop_singleLoad :: Property
prop_singleLoad = property $ do
  sDeps <- forAll $ genSyntheticDependencies (Range.linear 5 10)
  load1 <- forAll $ Gen.element (M.keys $ unSyntheticDependencies sDeps)
  let asyncConfig = mkAsyncLoaderConfig sDeps Default 2
  let initialLoaded = noResources
  let toLoad1 = S.singleton load1
  loaded <- liftIO $ withAsyncLoader asyncConfig () $
    \asyncLoader -> waitForResourceProcessing asyncLoader (S.empty, toLoad1)
  if M.size loaded > 0
  then pure ()
  else do
    vizFileName <- liftIO $ writeVizToFile (dependenciesToDot sDeps) 
    footnote $ "singleLoad failure loading resource \"" ++ load1 ++ "\", deps graph in file: " ++ vizFileName
    failure

-- | If you load a single resource and then unload it there should be no resources loaded at the end
prop_loadUnload1 :: Property
prop_loadUnload1 = property $ do
  sDeps <- forAll $ genSyntheticDependencies (Range.linear 5 10)
  load1 <- forAll $ Gen.element (M.keys $ unSyntheticDependencies sDeps)
  let asyncConfig = mkAsyncLoaderConfig sDeps Default 2
  let initialLoaded = noResources
  let toLoad1 = S.singleton load1
  unloaded <- liftIO $ withAsyncLoader asyncConfig () $ \asyncLoader -> do
    loaded <- waitForResourceProcessing asyncLoader   (S.empty, toLoad1)
    unloaded <- waitForResourceProcessing asyncLoader (toLoad1, S.empty)
    return unloaded
  diff unloaded (==) M.empty


-- | Load/unload using a loader with some load/unload delays, to help check for wierd race conditions
prop_loadUnloadSlow :: Property
prop_loadUnloadSlow = property $ do
  sDeps <- forAll $ genSyntheticDependencies (Range.linear 5 10)
  load1 <- forAll $ Gen.element (M.keys $ unSyntheticDependencies sDeps)
  let asyncConfig = mkAsyncLoaderConfig sDeps Slow 2
  let initialLoaded = noResources
  let toLoad1 = S.singleton load1
  unloaded <- liftIO $ withAsyncLoader asyncConfig () $ \asyncLoader -> do
    loaded <- waitForResourceProcessing asyncLoader (S.empty, toLoad1)
    unloaded <- waitForResourceProcessing asyncLoader (toLoad1, S.empty)
    return unloaded
  diff unloaded (==) M.empty

-- | synchronous loader
prop_syncSingleLoad :: Property
prop_syncSingleLoad = property $ do
  sDeps <- forAll $ genSyntheticDependencies (Range.linear 5 10)
  load1 <- forAll $ Gen.element (M.keys $ unSyntheticDependencies sDeps)
  let callbacks = mkLoaderCallbacks sDeps Default
  let initialLoaded = noResources
  loaded <- liftIO $ syncNewResources callbacks initialLoaded (S.singleton load1)
  assert (resourceCount loaded > 0)

-- | If you load a single resource and then unload it there should be no resources loaded at the end
prop_syncLoadUnload1 :: Property
prop_syncLoadUnload1 = property $ do
  sDeps <- forAll $ genSyntheticDependencies (Range.linear 5 10)
  load1 <- forAll $ Gen.element (M.keys $ unSyntheticDependencies sDeps)
  let callbacks = mkLoaderCallbacks sDeps Default
  let initialLoaded = noResources
  loaded <- liftIO $ syncNewResources callbacks initialLoaded (S.singleton load1)
  unloaded <- liftIO $ syncNewResources callbacks loaded S.empty
  assert (resourceCount unloaded == 0)

