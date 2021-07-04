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

import qualified Data.Map.Strict as M

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Generators
import TestViz
import LoadUnload

basicsTests :: IO Bool
basicsTests = checkSequential $$(discover)


-- | If you request to load a single resource the should be at least one resource loaded as a result
prop_singleLoad :: Property
prop_singleLoad = property $ do
  sDeps <- forAll $ genSyntheticDependencies (Range.linear 5 10)
  load1 <- forAll $ Gen.element (M.keys $ unSyntheticDependencies sDeps)
  let config = mkLoaderConfig sDeps Default
  loaded <- liftIO $ withTaskGroup 1 $ \tg -> fullLoad tg config noLoadedResources [load1]
  if (resourceCount loaded > 0)
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
  let config = mkLoaderConfig sDeps Default
  let initialLoaded = noLoadedResources
  loaded <- liftIO $ withTaskGroup 1 $ \tg -> fullLoad tg config initialLoaded [load1]
  unloaded <- liftIO $ withTaskGroup 1 $ \tg -> fullUnload tg config loaded [load1]
  assert (resourceCount unloaded == 0)


-- | Load/unload using a slow loader
prop_loadUnloadSlow :: Property
prop_loadUnloadSlow = property $ do
  sDeps <- forAll $ genSyntheticDependencies (Range.linear 5 10)
  load1 <- forAll $ Gen.element (M.keys $ unSyntheticDependencies sDeps)
  let config = mkLoaderConfig sDeps Slow
  let initialLoaded = noLoadedResources
  loaded <- liftIO $ withTaskGroup 1 $ \tg -> fullLoad tg config initialLoaded [load1]
  unloaded <- liftIO $ withTaskGroup 1 $ \tg -> fullUnload tg config loaded [load1]
  assert (resourceCount unloaded == 0)

-- | synchronous loader
prop_syncSingleLoad :: Property
prop_syncSingleLoad = property $ do
  sDeps <- forAll $ genSyntheticDependencies (Range.linear 5 10)
  load1 <- forAll $ Gen.element (M.keys $ unSyntheticDependencies sDeps)
  let config = mkLoaderConfig sDeps Default
  let initialLoaded = noLoadedResources
  loaded <- liftIO $ syncLoad config initialLoaded [load1]
  unloaded <- liftIO $ withTaskGroup 1 $ \tg -> fullUnload tg config loaded [load1]
  assert (resourceCount unloaded == 0)
