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
  mkLoaderConfig
  ) where

import Data.Foldable
import qualified Data.Map as M

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Lib

-- | A resource used for testing, with a few fields
data TestResource = TestResource !Int !Bool !Char
  deriving (Eq, Show)

-- | Generate a single 'TestResource'
genResource :: MonadGen m => m (String, TestResource)
genResource = do
  let keyLength = Range.linear 4 12
  let intRange = Range.linearFrom 0 (-5000) 5000
  k <- Gen.string keyLength Gen.alphaNum
  res <- TestResource <$> (Gen.integral intRange) <*> Gen.bool <*> Gen.unicode
  return (k,res)


-- | holds a bunch of resources and the things those resources depend on
newtype SyntheticDependencies = SyntheticDependencies { unSyntheticDependencies :: (M.Map String (TestResource, [String])) }
  deriving (Eq, Show)

-- | Generate a set of resources with dependencies. Dependencies can overlap but there are no
-- dependency cycles.
genSyntheticDependencies :: MonadGen m => Range.Range Int -> m SyntheticDependencies
genSyntheticDependencies resourceCountRange = do
  resList <- Gen.list resourceCountRange genResource
  resMap <- foldrM addResDeps M.empty resList
  return $ SyntheticDependencies resMap
  where
    addResDeps (k,r) m = do
      deps <- Gen.choice [
        return [],
        Gen.subsequence (M.keys m)
        ]
      return $ M.insert k (r,deps) m


data LoaderConfigSettings = Quiet | Noisy | Slow

mkLoaderConfig :: SyntheticDependencies -> LoaderConfigSettings -> Lib.ResourceLoaderConfig String TestResource
mkLoaderConfig (SyntheticDependencies depMap) _loaderSettings =
  ResourceLoaderConfig {
      loadIO = \l deps -> return (TestResource (3000 + length deps + length l) (null deps) 'a')
    , unloadIO = \_ _ -> do return ()
    , dependenciesIO = \l -> case M.lookup l depMap of
                               Nothing -> return []
                               Just (_,deps) -> return deps
  }

