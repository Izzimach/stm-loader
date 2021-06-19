{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec where

import Control.Concurrent
import Control.Concurrent.Async.Pool
import Control.Monad.IO.Class

import Data.Foldable
import Data.Maybe (fromJust)
import Data.Text.Lazy (pack)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Map as M

import Data.GraphViz
import Data.GraphViz.Attributes
import qualified Data.GraphViz.Attributes.HTML as HTML

import Lib

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
newtype SyntheticDependencies = SyntheticDependencies { unSyntheticDependencies :: (M.Map String (TestResource, [String])) }
  deriving (Eq, Show)

-- Generate a set of resources with dependencies. Dependencies can overlap but there are no
-- dependency cycles.
genSyntheticDependencies :: MonadGen m => Range.Range Int -> m SyntheticDependencies
genSyntheticDependencies resourceCount = do
  resList <- Gen.list resourceCount genResource
  resMap <- foldrM addResDeps M.empty resList
  return $ SyntheticDependencies resMap
  where
    addResDeps (k,r) m = do
      deps <- Gen.choice [
        return [],
        Gen.subsequence (M.keys m)
        ]
      return $ M.insert k (r,deps) m

data LoaderConfigSettings = Quiet | Noisy

mkLoaderConfig :: SyntheticDependencies -> LoaderConfigSettings -> Lib.ResourceLoaderConfig String TestResource
mkLoaderConfig (SyntheticDependencies depMap) loaderSettings =
  ResourceLoaderConfig {
      loadIO = \l deps -> return (TestResource (3000 + length deps + length l) (null deps) 'a')
    , unloadIO = \l r -> do return ()
    , dependenciesIO = \l -> case M.lookup l depMap of
                               Nothing -> return []
                               Just (_,deps) -> return deps
  }

-- | If you request to load a single resource the should be at least one resource loaded as a result
prop_singleLoad :: Property
prop_singleLoad = property $ do
  sDeps <- forAll $ genSyntheticDependencies (Range.linear 5 10)
  load1 <- forAll $ Gen.element (M.keys $ unSyntheticDependencies sDeps)
  let config = mkLoaderConfig sDeps Quiet
  let initialLoaded = noLoadedResources
  loaded <- liftIO $ withTaskGroup 1 $ \tg -> fullLoad tg config initialLoaded [load1]
  assert (resourceCount loaded > 0)

-- | If you load a single resource and then unload it there should be no resources loaded at the end
prop_loadUnload1 :: Property
prop_loadUnload1 = property $ do
  sDeps <- forAll $ genSyntheticDependencies (Range.linear 5 10)
  load1 <- forAll $ Gen.element (M.keys $ unSyntheticDependencies sDeps)
  let config = mkLoaderConfig sDeps Quiet
  let initialLoaded = noLoadedResources
  loaded <- liftIO $ withTaskGroup 4 $ \tg -> fullLoad tg config initialLoaded [load1]
  unloaded <- liftIO $ fullUnloadWait config loaded [load1]
  assert (resourceCount unloaded == 0)



--
-- visualization using graphviz
--

mkNodeLabel :: SyntheticDependencies -> (a, String) -> [Attribute]
mkNodeLabel (SyntheticDependencies deps) (n,l) =
        let (TestResource il bl cl) = fst $ fromJust $ M.lookup l deps
        in [
              toLabel $ HTML.Table $ HTML.HTable 
                                       (Just [HTML.Border 0, HTML.Title "Blargh", HTML.CellPadding 0, HTML.CellSpacing 0])
                                       [] 
                                       [
                                         HTML.Cells [HTML.LabelCell [HTML.Border 0] (HTML.Text [HTML.Str (pack l)])]
                                       , HTML.Cells [
                                                       HTML.LabelCell [] (HTML.Text [HTML.Str (pack $ show il)])
                                                     , HTML.LabelCell [] (HTML.Text [HTML.Str (pack $ show bl)])
                                                     , HTML.LabelCell [] (HTML.Text [HTML.Str (pack $ show cl)])
                                                    ]
                                        ]
            , shape PlainText
            ]

dependenciesToDot :: SyntheticDependencies -> DotGraph Int
dependenciesToDot sDeps@(SyntheticDependencies deps) =
  let synNodes = zip [1::Int ..] (M.keys deps)

      reverseLookup = M.fromList $ fmap (\(i,s) -> (s,i)) synNodes   -- map from node name to the int index of the nodes
      edgeData nodeName (_,depNodeNames) =
        let nodeIx = fromJust $ M.lookup nodeName reverseLookup
            depIxs = fmap (\k -> fromJust $ M.lookup k reverseLookup) depNodeNames
        in fmap (\x -> (nodeIx, x, "")) depIxs
      synEdges = concat $ M.elems $ M.mapWithKey edgeData deps :: [(Int,Int,String)]

      gParams = quickParams {
                  fmtNode = mkNodeLabel sDeps
                } :: GraphvizParams Int String String () String
  in
    graphElemsToDot gParams synNodes synEdges


writeGraphVizDependencies :: SyntheticDependencies -> FilePath -> IO ()
writeGraphVizDependencies sDeps fp = do
  let synthAsDot = dependenciesToDot sDeps
  _ <- runGraphvizCommand Dot synthAsDot Png fp
  return ()

