{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

--
-- Testing using a state machine to model a random sequence of loads and unlaods
--

module StateMachine (
  stateMachineTests
  ) where

import Control.Concurrent.Async.Pool
import Control.Monad.IO.Class

import Data.Kind
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Lib

import Generators

stateMachineTests :: IO Bool
stateMachineTests = checkSequential $$(discover)

--
-- load/unload state machine
--

-- test machine state tracks which resources are loaded or unloaded
data TestMachineState (v :: Type -> Type) = TestMachineState {
  loadedV :: [Var String v],
  synthDeps :: SyntheticDependencies,
  initialized :: Bool,
  stateRef :: Maybe (Var (Opaque (IORef (LoadedResources String TestResource))) v)
  } deriving (Eq)

instance (Show1 v) => Show (TestMachineState v) where
  show (TestMachineState lV sDep initP _) = "TestMachineState: loadedCount=" ++ show lV ++ " deps=" ++ show sDep ++ " initialized?: " ++ show initP

instance HTraversable TestMachineState where
  htraverse f (TestMachineState lV deps initP stRef) =
    TestMachineState 
      <$> traverse (htraverse f) lV
      <*> pure deps 
      <*> pure initP
      <*> traverse (htraverse f) stRef

initialMachineState :: SyntheticDependencies -> TestMachineState v
initialMachineState deps = TestMachineState [] deps False Nothing

loadable :: TestMachineState Concrete -> IO [String]
loadable (TestMachineState _ _ False _) = return []
loadable (TestMachineState _ _ _ Nothing) = return []
loadable (TestMachineState lV (SyntheticDependencies sd) True (Just _)) = do
  let possible = S.fromList (M.keys sd)
  let loaded = S.fromList $ fmap concrete lV
  return $ S.toList $ (possible `S.difference` loaded)

topLevelLoaded :: TestMachineState Concrete -> IO [String]
topLevelLoaded ts = do
  let sRef = opaque $ fromJust $ (stateRef ts)
  lRes <- liftIO $ readIORef sRef
  return $ S.toList (topLevelResources lRes)

unloadable :: TestMachineState (v :: Type -> Type) -> [Var String v]
unloadable ts = (loadedV ts)

allLoadableCount :: TestMachineState v -> Int
allLoadableCount ts = length (M.keys (unSyntheticDependencies $ synthDeps $ ts))

loadedCount :: TestMachineState v -> Int
loadedCount ts = length (loadedV ts)

data InitializeLoader (v :: Type -> Type) = InitializeLoader (TestMachineState v) deriving (Eq, Show)

instance HTraversable InitializeLoader where
  htraverse f (InitializeLoader ts) = InitializeLoader <$> htraverse f ts

data InitializeResult (v :: Type -> Type) = InitializeResult (IORef (LoadedResources String TestResource)) deriving (Eq)

instance (Show1 v) => Show (InitializeResult v) where
  show (InitializeResult _) = "InitializeResult [IORef]"

instance HTraversable InitializeResult where
  htraverse _ (InitializeResult x) = InitializeResult <$> pure x

s_initloader :: (MonadGen gen, MonadIO m) => LoadedResources String TestResource -> ResourceLoaderConfig String TestResource -> Command gen m TestMachineState
s_initloader initState _config = Command gen exec checks
  where
    gen ts = if (initialized ts)
             then Nothing
             else Just (InitializeLoader <$> Gen.constant ts)
    exec (InitializeLoader _) = do
      lRes <- liftIO (newIORef initState)
      return $ Opaque lRes
    checks = [
      Require (\s _ -> not $ initialized s),
      Update (\s _ lRes -> s { initialized = True, stateRef = Just $ lRes }),
      Ensure (\_s s' _i _o -> assert (initialized s'))
      ]
                         

data LoadGo (v :: Type -> Type) = LoadGo (TestMachineState v) Int deriving (Eq,Show)

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
                    Require (\s (LoadGo _ _l) -> (initialized s) && (loadedCount s < allLoadableCount s)),
                    Update (\s _i l -> s { loadedV = l : (loadedV s) }),
                    Ensure (\s s' _i o ->
                              do
                                assert (not $ elem o (fmap concrete (loadedV s)))
                                assert (elem o (fmap concrete (loadedV s')))
                                assert (loadedCount s' > loadedCount s)
                                assert (loadedCount s' <= allLoadableCount s))
                  ]

data UnloadGo (v :: Type -> Type) = UnloadGo (TestMachineState v) (Var String v) deriving (Eq,Show)

instance HTraversable UnloadGo where
  htraverse f (UnloadGo ts ul) = UnloadGo <$> htraverse f ts <*> htraverse f ul

s_unload :: (MonadGen gen, MonadIO m) => ResourceLoaderConfig String TestResource -> Command gen m TestMachineState
s_unload config = Command gen exec checks
  where
    gen s = if (not $ initialized s)
            then Nothing
            else case (unloadable s) of
                  [] -> Nothing
                  a -> Just $ UnloadGo <$> Gen.constant s <*> Gen.element a
    exec (UnloadGo ts ul) = do
      let ulc = concrete ul
      let sRef = opaque $ fromJust $ (stateRef ts)
      lRes <- liftIO $ readIORef sRef
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

data SampleTL (v :: Type -> Type) = SampleTL (TestMachineState v) deriving (Eq,Show)

instance HTraversable SampleTL where
  htraverse f (SampleTL ts) = SampleTL <$> htraverse f ts


s_sampleTLCount :: (MonadGen gen, MonadIO m) => ResourceLoaderConfig String TestResource -> Command gen m TestMachineState
s_sampleTLCount _config = Command gen exec checks
  where
    gen s = if (not $ initialized s)
            then Nothing
            else Just $ SampleTL <$> Gen.constant s
    exec (SampleTL ts) = do
      let sRef = opaque $ fromJust $ (stateRef ts)
      lRes <- liftIO $ readIORef sRef
      return (S.toList $ topLevelResources lRes)
    checks = [
               Ensure (\s _s' _ tlrs -> do
                 footnote $ show tlrs ++ " / " ++ show (loadedV s)
                 length tlrs === length (loadedV s))
             ]

actionGen :: (MonadGen gen, MonadTest m, MonadIO m) => gen (SyntheticDependencies, Sequential m TestMachineState)
actionGen = do
  sDeps <- genSyntheticDependencies (Range.linear 8 12)
  let config = mkLoaderConfig sDeps Quiet
  let commands = [
        s_initloader noLoadedResources config,
        s_load config,
        s_unload config,
        s_sampleTLCount config
        ]
  actions <- Gen.sequential (Range.linear 3 5) (initialMachineState sDeps) commands
  return (sDeps,actions)
  
prop_StateMachineTest :: Property
prop_StateMachineTest = property $ do
  (sDeps, actions) <- forAll actionGen
  executeSequential (initialMachineState sDeps) actions

