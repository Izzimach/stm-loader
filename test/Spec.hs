{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec where

import Control.Concurrent
import Control.Concurrent.Async.Pool
import Control.Monad.IO.Class

import Data.Foldable
import Data.IORef
import Data.Maybe (fromJust)
import Data.Text.Lazy (pack)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Map as M
import qualified Data.Set as S

import Data.GraphViz
import Data.GraphViz.Attributes
import qualified Data.GraphViz.Attributes.HTML as HTML
import qualified Data.GraphViz.Attributes.Colors.X11 as X11Color

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


data LoaderConfigSettings = Quiet | Noisy | Slow

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
-- load/unload state machine
--

-- test machine state tracks which resources are loaded or unloaded
data TestMachineState (v :: * -> *) = TestMachineState {
  loadedV :: [Var String v],
  synthDeps :: SyntheticDependencies,
  initialized :: Bool,
  stateRef :: Maybe (Var (Opaque (IORef (LoadedResources String TestResource))) v)
  } deriving (Eq)

instance (Show1 v) => Show (TestMachineState v) where
  show (TestMachineState lV sDep init _) = "TestMachineState: loadedCount=" ++ show lV ++ " deps=" ++ show sDep ++ " initialized?: " ++ show init

instance HTraversable TestMachineState where
  htraverse f (TestMachineState lV deps init stRef) =
    TestMachineState 
      <$> traverse (htraverse f) lV
      <*> pure deps 
      <*> pure init
      <*> traverse (htraverse f) stRef

initialMachineState :: SyntheticDependencies -> TestMachineState v
initialMachineState deps = TestMachineState [] deps False Nothing

loadable :: TestMachineState Concrete -> IO [String]
loadable (TestMachineState _ _ False _) = return []
loadable (TestMachineState _ _ _ Nothing) = return []
loadable (TestMachineState lV (SyntheticDependencies sd) True (Just stRef)) = do
  let possible = S.fromList (M.keys sd)
  let loaded = S.fromList $ fmap concrete lV
  return $ S.toList $ (possible `S.difference` loaded)

topLevelLoaded :: TestMachineState Concrete -> IO [String]
topLevelLoaded ts = do
  let sRef = opaque $ fromJust $ (stateRef ts)
  lRes <- liftIO $ readIORef sRef
  return $ S.toList (topLevelResources lRes)

unloadable :: TestMachineState (v :: * -> *) -> [Var String v]
unloadable ts = (loadedV ts)

allLoadableCount :: TestMachineState v -> Int
allLoadableCount ts = length (M.keys (unSyntheticDependencies $ synthDeps $ ts))

loadedCount :: TestMachineState v -> Int
loadedCount ts = length (loadedV ts)

data InitializeLoader (v :: * -> *) = InitializeLoader (TestMachineState v) deriving (Eq, Show)

instance HTraversable InitializeLoader where
  htraverse f (InitializeLoader ts) = InitializeLoader <$> htraverse f ts

data InitializeResult (v :: * -> *) = InitializeResult (IORef (LoadedResources String TestResource)) deriving (Eq)

instance (Show1 v) => Show (InitializeResult v) where
  show (InitializeResult x) = "InitializeResult [IORef]"

instance HTraversable InitializeResult where
  htraverse _ (InitializeResult x) = InitializeResult <$> pure x

s_initloader :: (MonadGen gen, MonadTest m, MonadIO m) => LoadedResources String TestResource -> ResourceLoaderConfig String TestResource -> Command gen m TestMachineState
s_initloader initState config = Command gen exec checks
  where
    gen ts = if (initialized ts)
             then Nothing
             else Just (InitializeLoader <$> Gen.constant ts)
    exec (InitializeLoader ts) = do
      lRes <- liftIO (newIORef initState)
      return $ Opaque lRes
    checks = [
      Require (\s _ -> not $ initialized s),
      Update (\s i lRes -> s { initialized = True, stateRef = Just $ lRes }),
      Ensure (\s s' i o -> assert (initialized s'))
      ]
                         

data LoadGo (v :: * -> *) = LoadGo (TestMachineState v) Int deriving (Eq,Show)

instance HTraversable LoadGo where
  htraverse f (LoadGo ts i) = LoadGo <$> htraverse f ts <*> pure i

s_load :: (MonadGen gen, MonadTest m, MonadIO m) => ResourceLoaderConfig String TestResource -> Command gen m TestMachineState
s_load config = Command
                  (\s -> if (not $ initialized s)
                         then Nothing
                         else if (loadedCount s >= allLoadableCount s)  -- no more things we can load
                              then Nothing
                              else Just (LoadGo <$> Gen.constant s <*> Gen.int (Range.linear 0 5)))
                  (\(LoadGo ts ix) -> do
                      -- use ix to pick the thing to load
                      let sRef = opaque $ fromJust $ (stateRef ts)
                      lRes <- liftIO $ readIORef sRef
                      loadChoices <- liftIO $ loadable ts
                      let loadix = mod ix (length loadChoices)
                      let l = loadChoices !! loadix
                      lRes' <- liftIO $ withTaskGroup 1 $ \tg -> fullLoad tg config lRes [l]
                      liftIO $ writeIORef sRef lRes'
                      return l
                  )
                  [
                    Require (\s (LoadGo _ l) -> (initialized s) && (loadedCount s < allLoadableCount s)),
                    Update (\s i l -> s { loadedV = l : (loadedV s) }),
                    Ensure (\s s' i o ->
                              do
                                assert (not $ elem o (fmap concrete (loadedV s)))
                                assert (elem o (fmap concrete (loadedV s')))
                                assert (loadedCount s' > loadedCount s)
                                assert (loadedCount s' <= allLoadableCount s))
                  ]

data UnloadGo (v :: * -> *) = UnloadGo (TestMachineState v) (Var String v) deriving (Eq,Show)

instance HTraversable UnloadGo where
  htraverse f (UnloadGo ts ul) = UnloadGo <$> htraverse f ts <*> htraverse f ul

s_unload :: (MonadGen gen, MonadTest m, MonadIO m) => ResourceLoaderConfig String TestResource -> Command gen m TestMachineState
s_unload config = Command gen exec checks
  where
    gen s = if (not $ initialized s)
            then Nothing
            else case (unloadable s) of
                  [] -> Nothing
                  a@(x:xs) -> Just $ UnloadGo <$> Gen.constant s <*> Gen.element a
    exec (UnloadGo ts ul) = do
      let ulc = concrete ul
      let sRef = opaque $ fromJust $ (stateRef ts)
      lRes <- liftIO $ readIORef sRef
      loadChoices <- liftIO $ loadable ts
      lRes' <- liftIO $ withTaskGroup 1 $ \tg -> fullUnload tg config lRes [ulc]
      liftIO $ writeIORef sRef lRes'
      return ulc
    checks = [
                Require (\s (UnloadGo _ ul) -> (initialized s) && (elem ul (loadedV s))),
                Update (\s (UnloadGo _ ul) _ -> s { loadedV = filter (/= ul) (loadedV s) }),
                Ensure (\s s' (UnloadGo _ ul) o -> do
                          concrete ul === o
                          footnote $ show o ++ " " ++ show (fmap concrete (loadedV s))
                          assert $ elem o $ fmap concrete (loadedV s)
                          assert $ not $ elem o $ fmap concrete (loadedV s')
                          assert (loadedCount s' < loadedCount s))
              ]

data SampleTL (v :: * -> *) = SampleTL (TestMachineState v) deriving (Eq,Show)

instance HTraversable SampleTL where
  htraverse f (SampleTL ts) = SampleTL <$> htraverse f ts


s_sampleTLCount :: (MonadGen gen, MonadTest m, MonadIO m) => ResourceLoaderConfig String TestResource -> Command gen m TestMachineState
s_sampleTLCount config = Command gen exec checks
  where
    gen s = if (not $ initialized s)
            then Nothing
            else Just $ SampleTL <$> Gen.constant s
    exec (SampleTL ts) = do
      let sRef = opaque $ fromJust $ (stateRef ts)
      lRes <- liftIO $ readIORef sRef
      return (length (topLevelResources lRes))
    checks = [
               Ensure (\s s' _ tlc -> assert $ tlc == length (loadedV s))
             ]


prop_StateMachineTest :: Property
prop_StateMachineTest = property $ do
  sDeps <- forAll $ genSyntheticDependencies (Range.linear 8 20)
  let config = mkLoaderConfig sDeps Quiet
  let commands = [
        s_initloader noLoadedResources config,
        s_load config,
        s_unload config,
        s_sampleTLCount config
        ]
  actions <- forAll $ Gen.sequential (Range.linear 5 12) (initialMachineState sDeps) commands
  executeSequential (initialMachineState sDeps) actions

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