{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TestViz where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.Text.Lazy (pack)

import Data.GraphViz
import qualified Data.GraphViz.Attributes.HTML as HTML
import qualified Data.GraphViz.Attributes.Colors.X11 as X11Color

import LoadUnload
import Generators

--
-- visualization using graphviz
--

mkNodeLabel :: SyntheticDependencies -> (a, String) -> [Attribute]
mkNodeLabel (SyntheticDependencies deps) (_,l) =
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
mkResourceInfoLabel (LoadedResources rMap topLevel) (_,name) =
        let (ResourceInfo lKey val _ _) = fromJust $ M.lookup name rMap
            loadKeyLabel = pack $ show lKey
            valueLabel = pack $ show val
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
vizLoadedResources lr@(LoadedResources rMap _topLevel) =
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
