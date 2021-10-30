{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec where

import Control.Monad.IO.Class

import qualified Data.Set as S

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Map.Strict as M


import STMLoader.LoadUnload

import Generators
import Basics
import StateMachine

mainTest :: IO Bool
mainTest = do
  test1 <- basicsTests
  test2 <- stateMachineTests
  return (test1 && test2)

-- for REPL testing of loading
runNLoads :: Int -> IO (SyntheticDependencies, ResourcesMap String TestResource)
runNLoads n = do
  sDeps <- Gen.sample $ genSyntheticDependencies (Range.linear (n*2) (n*3))
  loadN <- fmap (S.fromList . take n) $ Gen.sample $ Gen.shuffle (M.keys $ unSyntheticDependencies sDeps)
  let config = mkLoaderCallbacks sDeps Default
  let initialLoaded = noResources
  loaded <- liftIO $ syncNewResources config initialLoaded loadN
  return (sDeps, loaded)


