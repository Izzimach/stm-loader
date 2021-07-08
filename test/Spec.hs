{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec where

import Control.Concurrent.Async.Pool
import Control.Monad.IO.Class

import Data.Maybe (fromJust)
import Data.Text.Lazy (pack)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Map.Strict as M


import LoadUnload

import Generators
import Basics
import StateMachine

mainTest :: IO Bool
mainTest = do
  test1 <- basicsTests
  test2 <- stateMachineTests
  return (test1 && test2)

-- for REPL testing of loading
runNLoads :: Int -> IO (SyntheticDependencies, LoadedResources String TestResource)
runNLoads n = do
  sDeps <- Gen.sample $ genSyntheticDependencies (Range.linear (n*2) (n*3))
  loadN <- fmap (take n) $ Gen.sample $ Gen.shuffle (M.keys $ unSyntheticDependencies sDeps)
  let config = mkLoaderConfig sDeps Default
  let initialLoaded = noLoadedResources
  loaded <- liftIO $ withTaskGroup 1 $ \tg -> fullLoad tg config initialLoaded loadN
  return (sDeps, loaded)


