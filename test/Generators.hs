{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Generators (
  TestResource(..),
  genResource,
  SyntheticDependencies(..),
  genSyntheticDependencies,
  LoaderConfigSettings(..),
  mkLoaderCallbacks,
  mkAsyncLoaderConfig
  ) where

import Control.Concurrent

import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M

import Hedgehog
import Hedgehog.Corpus (animals)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import STMLoader.LoadUnload
import STMLoader.AsyncLoader

-- | A resource used for testing, with a few fields
data TestResource = TestResource !Int !Bool !Char
  deriving (Eq, Show)

type ErrorMessage = String

-- | Generate a single 'TestResource'
genResource :: MonadGen m => m TestResource
genResource = 
  let intRange = Range.linearFrom 0 (-5000) 5000
  in TestResource <$> Gen.integral intRange <*> Gen.bool <*> Gen.unicode


-- | holds a bunch of resources and the things those resources depend on
newtype SyntheticDependencies = SyntheticDependencies { unSyntheticDependencies :: M.Map String (TestResource, Set String) }
  deriving (Eq, Show)

-- | Generate a set of resources with dependencies. Dependencies can overlap but there are no
-- dependency cycles.
genSyntheticDependencies :: MonadGen m => Range.Range Int -> m SyntheticDependencies
genSyntheticDependencies resourceCountRange = do
  resList <- Gen.list resourceCountRange genResource
  -- We use a shuffled predefined set of keys. Generating random strings of characters would give more
  -- variety but explodes the amount of choices when Hedgehog has to shrink.
  resKeys <- Gen.shuffle Hedgehog.Corpus.animals
  resMap <- foldrM addResDeps M.empty (zip resKeys resList)
  return $ SyntheticDependencies resMap
  where
    -- when we add a resource we sometimes choose some of the previously added resources as dependencies
    addResDeps (k,r) m = do
      deps <- Gen.choice [
        return S.empty,                  -- no dependencies
        S.fromList <$> Gen.subsequence (M.keys m)  -- some dependencies
        ]
      return $ M.insert k (r,deps) m


-- | Lets you set up different laoded simulations
data LoaderConfigSettings =
    Default -- the default, no delays in loading
  | Slow    -- each action (load,unload, dependencies) incurs a delay

mkLoaderCallbacks :: SyntheticDependencies -> LoaderConfigSettings -> LoadUnloadCallbacks String TestResource ErrorMessage
mkLoaderCallbacks (SyntheticDependencies synDeps) loaderSettings =
  let doDelay = case loaderSettings of
                  Default -> (\_ -> return ()) -- no-op
                  Slow    -> (\a -> threadDelay (2000+100*a))
  in
    LoadUnloadCallbacks {
        loadResource = \l depMap -> do
          doDelay $ length depMap
          let deps = M.keys depMap
          return (TestResource (3000 + length deps + length l) (null deps) 'a') 
      , unloadResource = \_ -> do
          doDelay 5
          return ()
      , findDependencies = \l -> do
          doDelay $ length l
          case M.lookup l synDeps of
            Nothing -> return S.empty
            Just (_,deps) -> return deps
      , processException = const $ Report "Error!"
    }

mkAsyncLoaderConfig :: SyntheticDependencies -> LoaderConfigSettings -> Int -> AsyncLoaderConfig String TestResource () ErrorMessage
mkAsyncLoaderConfig synDeps settings workers =
  AsyncLoaderConfig
  {
      callbackFunctions = mkLoaderCallbacks synDeps settings
    , workerCount = workers
    , forkMarshal = simpleFork
    , forkLoadWorker = simpleFork
  }
  where
    simpleFork :: ThreadWrapper String TestResource () ErrorMessage
    simpleFork = ThreadWrapper (\_ p config -> forkIO (p config))

