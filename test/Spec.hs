{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec where

import Data.Foldable

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Map as M


main :: IO Bool
main = checkSequential $$(discover)
  

-- Generate a single key and resource
data TestResource = TestResource !Int !Bool !Char
  deriving (Eq, Show)

genResource :: MonadGen m => m (String, TestResource)
genResource = do
  let keyLength = Range.linear 3 12
  let intRange = Range.linearFrom 0 (-5000) 5000
  k <- Gen.string keyLength Gen.alphaNum
  res <- TestResource <$> (Gen.integral intRange) <*> Gen.bool <*> Gen.unicode
  return (k,res)

-- holds a bunch of resources and the things those resources depend on
newtype TestResourceDeps = TestResourceDeps { unTestResourceDeps :: (M.Map String (TestResource, [String])) }
  deriving (Eq, Show)

-- Generate a set of resources with dependencies. Dependencies can overlap but there are no
-- dependency cycles.
genResourcesWithDeps :: MonadGen m => Range.Range Int -> m TestResourceDeps
genResourcesWithDeps resourceCount = do
  resList <- Gen.list resourceCount genResource
  resMap <- foldrM addResDeps M.empty resList
  return $ TestResourceDeps resMap
  where
    addResDeps (k,r) m = do
      deps <- Gen.choice [
        return [],
        Gen.subsequence (M.keys m)
        ]
      return $ M.insert k (r,deps) m



prop_someThing :: Property
prop_someThing = property $ do
  res <- forAll $ genResourcesWithDeps (Range.linear 10 20)
  cover 1 ("less than 13 resources" :: LabelName) (length (unTestResourceDeps res) < 13)
  pure ()


