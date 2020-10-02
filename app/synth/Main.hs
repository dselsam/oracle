{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Generate synthetic data for `([Bool], [Int]) -> Int` inductive synthesis problems.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Console.CmdArgs

import Oracle.Control.Monad.Search
import Oracle.Search.BruteForce

import Oracle.Search.Decision (Decision(Decision))
import qualified Oracle.Search.Decision as Decision
import qualified Oracle.Search.Result as Result
import qualified Oracle.Search.Replay as Search

import Oracle.Examples.Synth
import qualified Oracle.Examples.Synth.Features as Features
import qualified Oracle.Examples.Synth.TTSInfo as TTSInfo
import qualified Oracle.Examples.Synth.TTS as TTS
import qualified Oracle.Examples.Synth.Specs.ESpec as ESpec
import qualified Oracle.Examples.Synth.Basic as Synth
import qualified Oracle.Examples.Synth.Ints2Int as Synth
import qualified Oracle.Examples.Synth.DecisionTree as Synth
import qualified Oracle.Examples.Synth.DecisionTree as DTree
import qualified Oracle.Examples.Synth.SynthContext as SynthContext

import qualified Oracle.Neural.Sequence as S2S

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import qualified Oracle.Util.List as List

import Data.Maybe (fromJust, isJust)
import GHC.Exts (toList)
import Test.Hspec
import Control.Monad (when, unless, guard, replicateM)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (State, get, gets, modify, execState)
import Data.Foldable (for_)
import Data.Sequence (Seq, (|>), (><))
import qualified Data.Sequence as Seq

import System.Random
import qualified Data.List as List

data Args = Args {
  train           :: Bool,
  trainFilename   :: FilePath,
  nDTrees         :: Int,
  nBoolFeatures   :: Int,
  nIntFeatures    :: Int,
  nTrain          :: Int,
  nTest           :: Int,
  synthFuel       :: Int
  -- TODO: predict args
  } deriving (Show, Data, Typeable)

defaultArgs = Args {
  train           = True,
  trainFilename   = "synth-sequence-training-data.txt",
  nDTrees         = 10000,
  nBoolFeatures   = 4,
  nIntFeatures    = 4,
  nTrain          = 6,
  nTest           = 2,
  synthFuel       = 4
  }

sampleInt :: Int -> Int -> IO Int
sampleInt low high = getStdRandom $ randomR (low, high)

sampleBool :: IO Bool
sampleBool = getStdRandom $ randomR (False, True)

data FeaturesSpec = FeaturesSpec {
  _info   :: TTSInfo,
  _nBools :: Int,
  _nInts  :: Int
  }

type Inputs = (Features Bool, Features Int)

sampleFeatures :: FeaturesSpec -> IO Inputs
sampleFeatures (FeaturesSpec (TTSInfo nTrain nTest) nBools nInts) = do
  bools <- flip mapM [1..nBools] $ \i -> do
    train <- replicateM nTrain sampleBool
    test  <- replicateM nTest sampleBool
    pure ("bool" ++ show i, TTS train test)
  ints <- flip mapM [1..nInts] $ \i -> do
    train <- replicateM nTrain $ sampleInt 1 10
    test  <- replicateM nTest $ sampleInt 1 10
    pure ("int" ++ show i, TTS train test)
  pure (Features bools, Features ints)

type FeatureIdx = Int

data ArithProg = ArithId FeatureIdx
               | ArithConst Int
               | Add FeatureIdx ArithProg
               | Mul FeatureIdx ArithProg
               | Div1 FeatureIdx ArithProg
               | Div2 FeatureIdx ArithProg
               deriving (Eq, Ord)

instance Show ArithProg where
  show (ArithId i)     = "#" ++ show i
  show (ArithConst c)  = show c
  show (Add  i prog)   = "(+ #" ++ show i ++ " " ++ show prog ++ ")"
  show (Mul  i prog)   = "(* #" ++ show i ++ " " ++ show prog ++ ")"
  show (Div1  i prog)  = "(/ #" ++ show i ++ " " ++ show prog ++ ")"
  show (Div2  i prog)  = "(/ " ++ show prog ++ " #" ++ show i ++ ")"

mkArithProgs :: Int -> Int -> Seq ArithProg
mkArithProgs maxResults nIntFeatures = runIdentity $ do
  let fuel = 100000000
  let opts = BruteForceOptions maxResults fuel BreadthFirst
  results <- bruteForceSearch opts enum
  pure $ fmap Result.value results

  where
    enum :: SearchT Identity ArithProg
    enum = choice "" [
      ("", ArithId <$> chooseFeature),
      ("", ArithConst <$> chooseConst),
      ("", Add  <$> chooseFeature <*> enum),
      ("", Mul  <$> chooseFeature <*> enum),
      ("", Div1 <$> chooseFeature <*> enum),
      ("", Div2 <$> chooseFeature <*> enum)
      ]

    chooseFeature :: SearchT Identity FeatureIdx
    chooseFeature = oneOfSelf "" [0..(nIntFeatures - 1)]

    chooseConst :: SearchT Identity Int
    chooseConst = oneOfSelf "" [1..11]

evalArithProg :: Features Int -> ArithProg -> Maybe (TTS Int)
evalArithProg ints prog = case prog of
  ArithId i    -> pure $ Features.getTTS i ints
  ArithConst k -> pure $ TTS.replicate (Features.getInfo ints) k
  Add i prog -> do
    let x = Features.getTTS i ints
    y <- evalArithProg ints prog
    pure $ fmap (\(x, y) -> x + y) (TTS.zip x y)
  Mul i prog -> do
    let x = Features.getTTS i ints
    y <- evalArithProg ints prog
    pure $ fmap (\(x, y) -> x * y) (TTS.zip x y)
  Div1 i prog -> do
    let x = Features.getTTS i ints
    y <- evalArithProg ints prog
    guard $ all (/= 0) y && all ((==0) . uncurry mod) (TTS.zip x y)
    pure $ fmap (\(x, y) -> div x y) (TTS.zip x y)
  Div2 i prog -> do
    x <- evalArithProg ints prog
    let y = Features.getTTS i ints
    guard $ all (/= 0) y && all ((==0) . uncurry mod) (TTS.zip x y)
    pure $ fmap (\(x, y) -> div x y) (TTS.zip x y)

-- TODO: error-prone labeling, missing abstractions
labelArithProg :: ArithProg -> Features Int -> State (Seq Int) ()
labelArithProg prog ints = case prog of
  ArithId i    -> modify (|> 0) *> modify (|> 0) *> modify (|> i)
  ArithConst k -> modify (|> 0) *> modify (|> 1) *> modify (|> 0)
  Add  i prog  -> modify (|> 1) *> modify (|> i) *> modify (|> 0) *> labelArithProg prog ints
  Mul  i prog  -> modify (|> 1) *> modify (|> i) *> modify (|> 1) *> labelArithProg prog ints
  Div1 i prog  -> modify (|> 1) *> modify (|> i) *> modify (|> 2) *> labelArithProg prog ints
  Div2 i prog  -> modify (|> 1) *> modify (|> i) *> modify (|> 3) *> labelArithProg prog ints

data DTree = Leaf ArithProg
           | Node FeatureIdx DTree DTree
           deriving (Eq, Ord, Show)

mkDTrees :: Int -> Int -> Int -> Seq DTree
mkDTrees maxResults nBoolFeatures nIntFeatures = runIdentity $ do
  let fuel = 100000000000
  let opts = BruteForceOptions maxResults fuel BreadthFirst
  results <- bruteForceSearch opts enum
  pure $ fmap Result.value results

  where
    enum :: SearchT Identity DTree
    enum = choice "" [
      ("", Leaf <$> chooseArithProg),
      ("", Node <$> chooseFeature <*> enum <*> enum)
      ]

    chooseArithProg :: SearchT Identity ArithProg
    chooseArithProg = Seq.index arithProgs <$> oneOfSelf "" [0..(Seq.length arithProgs - 1)]

    chooseFeature :: SearchT Identity FeatureIdx
    chooseFeature = oneOfSelf "" [0..(nBoolFeatures - 1)]

    arithProgs = mkArithProgs (div maxResults 100) nIntFeatures

evalDTree :: Inputs -> DTree -> Maybe (TTS Int)
evalDTree (bools, ints) dtree = guard (not . Features.isEmpty $ bools) *> case dtree of
  Leaf prog -> evalArithProg ints prog
  Node i dtreeT dtreeF -> do
    let b = Features.getTTS i bools
    let (infoTrue, infoFalse) = TTS.partitionInfosOn b
    guard $ all okInfo [infoTrue, infoFalse]
    let (boolsTrue, boolsFalse) = SynthContext.partitionOn b bools
    let (intsTrue,  intsFalse ) = SynthContext.partitionOn b ints
    trueGuesses  <- evalDTree (boolsTrue,  intsTrue) dtreeT
    falseGuesses <- evalDTree (boolsFalse, intsFalse) dtreeF
    pure $ TTS.unpartitionOn b trueGuesses falseGuesses

  where
    okInfo (TTSInfo nTrain _) = nTrain > 0


labelDTree :: DTree -> Inputs -> State (Seq Int) ()
labelDTree dtree0 inputs0 = core dtree0 spec0 where
  core dtree spec = case dtree of
    Leaf prog -> do
      -- Note: assumes the depth will be high enough that this never hits basecase
      -- Could also restructure the synthesizer to be more uniform
      modify (|> 0)
      labelArithProg prog $ snd (ESpec.ctx spec)
    Node i dtreeT dtreeF -> do
      modify (|> 1)
      modify (|> i)
      let (specT, specF) = DTree.splitSpec (Features.getTTS i (fst $ ESpec.ctx spec)) spec
      core dtreeT specT
      core dtreeF specF

  spec0   = ESpec info0 inputs0 labels0
  info0   = Features.getInfo (fst inputs0)

  -- Not actually using labels, just convenient to get splitSpec
  labels0 = case evalDTree inputs0 dtree0 of
    Just outputs0 -> TTS.train outputs0

main :: IO ()
main = do
  putStrLn "[synth-exe]"
  args <- cmdArgs defaultArgs
  let info = TTSInfo (nTrain args) (nTest args)

  (bools, ints) <- sampleFeatures (FeaturesSpec info (nBoolFeatures args) (nIntFeatures args))
  let dtrees   = toList $ mkDTrees (nDTrees args) (nBoolFeatures args) (nIntFeatures args)
  let dOutputs = map (evalDTree (bools, ints)) dtrees
  let choicess = flip map dtrees $ \dtree -> toList $ flip execState Seq.empty $ labelDTree dtree (bools, ints)

  (dtreesChoices, dOutputs) <- pure . unzip . List.stableUniqKey (TTS.train . fromJust . snd) . filter (isJust . snd) $ zip (zip dtrees choicess) dOutputs
  let (dtrees, choicess) = unzip dtreesChoices

  for_ (List.zip4 [1..] dtrees choicess dOutputs) $ \(i, dtree, choices, dOutput) -> do
    let spec  = ESpec info (bools, ints) (TTS.train $ fromJust dOutput)
    let answer = runIdentity $ Search.replay (Synth.decisionTreeNaive (synthFuel args) (Synth.ints2int $ synthFuel args) spec) choices

    for_ answer $ \(Decision snapshot choices choiceIdx _) -> do
      for_ (zip [0..] (toList choices)) $ \(i, choice) -> do
        S2S.appendTokensToFile (trainFilename args) $ Seq.fromList [if i == choiceIdx then "T" else "F", "SNAPSHOT"] >< S2S.embed snapshot >< Seq.singleton "CHOICE" >< S2S.embed choice
