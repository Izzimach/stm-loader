{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent.Async.Pool
import Control.Monad.IO.Class

import Data.Foldable
import Data.Maybe (fromJust)
import Data.Text.Lazy (pack)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Map as M
import qualified Data.Set as S

import Data.GraphViz
import qualified Data.GraphViz.Attributes.HTML as HTML
import qualified Data.GraphViz.Attributes.Colors.X11 as X11Color

import Lib

import Generators
import StateMachine

main :: IO Bool
main = do
  test1 <- checkSequential $$(discover)
  test2 <- stateMachineTests
  return (test1 && test2)
  


-- | If you request to load a single resource the should be at least one resource loaded as a result
prop_singleLoad :: Property
prop_singleLoad = property $ do
  sDeps <- forAll $ genSyntheticDependencies (Range.linear 5 10)
  load1 <- forAll $ Gen.element (M.keys $ unSyntheticDependencies sDeps)
  let config = mkLoaderConfig sDeps Quiet
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
  let config = mkLoaderConfig sDeps Quiet
  let initialLoaded = noLoadedResources
  loaded <- liftIO $ withTaskGroup 1 $ \tg -> fullLoad tg config initialLoaded [load1]
  unloaded <- liftIO $ withTaskGroup 1 $ \tg -> fullUnload tg config loaded [load1]
  assert (resourceCount unloaded == 0)


-- for REPL testing of loading
runNLoads :: Int -> IO (SyntheticDependencies, LoadedResources String TestResource)
runNLoads n = do
  sDeps <- Gen.sample $ genSyntheticDependencies (Range.linear (n*2) (n*3))
  loadN <- fmap (take n) $ Gen.sample $ Gen.shuffle (M.keys $ unSyntheticDependencies sDeps)
  let config = mkLoaderConfig sDeps Quiet
  let initialLoaded = noLoadedResources
  loaded <- liftIO $ withTaskGroup 1 $ \tg -> fullLoad tg config initialLoaded loadN
  return (sDeps, loaded)


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




--
-- graphviz of LoadedResources
--


mkResourceInfoLabel :: (Show r) => LoadedResources String r -> (Int, String) -> [Attribute]
mkResourceInfoLabel (LoadedResources rMap topLevel) (ix,name) =
        let (ResourceInfo loadKey value _ _) = fromJust $ M.lookup name rMap
            loadKeyLabel = pack $ show loadKey
            valueLabel = pack $ show value
            isTopLevel = S.member name topLevel
            border = if isTopLevel then HTML.Border 2 else HTML.Border 1
            titleColor = if isTopLevel then X11Color.Blue else X11Color.Black
        in [
              toLabel $
                HTML.Table $ 
                  HTML.HTable 
                    Nothing
                    [border, HTML.CellBorder 1, HTML.CellPadding 0, HTML.CellSpacing 0]
                    [
                      HTML.Cells [HTML.LabelCell [HTML.Border 0] (HTML.Text [HTML.Str loadKeyLabel])]
                    , HTML.Cells [
                                   HTML.LabelCell [HTML.CellPadding 0, HTML.CellSpacing 0] (HTML.Text [HTML.Str valueLabel])
                                 ]
                    ]
            , shape PlainText
            , color $ titleColor
            ]

vizLoadedResources :: (Show r) => LoadedResources String r -> DotGraph Int
vizLoadedResources lr@(LoadedResources rMap topLevel) =
  let resNodes = zip [1..] (M.keys rMap)
      reverseLookup = M.fromList $ fmap (\(i,s) -> (s,i)) resNodes   -- map from node name to the int index of the nodes
      edgeData nodeName (ResourceInfo _ _ depOn _) =
        let nodeIx = fromJust $ M.lookup nodeName reverseLookup
            depIxs = fmap (\k -> fromJust $ M.lookup k reverseLookup) (S.toList depOn)
        in fmap (\x -> (nodeIx, x, "")) depIxs
      resEdges = concat $ M.elems $ M.mapWithKey edgeData rMap :: [(Int,Int,String)]

      gParams = quickParams {
              fmtNode = mkResourceInfoLabel lr
            } :: GraphvizParams Int String String () String
  in
    graphElemsToDot gParams resNodes resEdges











-- | Given a dot graph, writes the viz output to a specific file. Returns the file name.
writeVizToFile :: DotGraph Int -> IO FilePath
writeVizToFile dg = do
  let fp = "testViz.png"
  _ <- runGraphvizCommand Dot dg Png fp
  return fp