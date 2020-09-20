{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Generate synthetic data for `([Bool], [Int]) -> Int` inductive synthesis problems.
-}

module Main where

import Oracle.SearchT
import Oracle.Search.BruteForce

import Oracle.Search.Decision (Decision(Decision))
import qualified Oracle.Search.Decision as Decision
import qualified Oracle.Search.Result as Result

import Oracle.Examples.Synth
import qualified Oracle.Examples.Synth.Features as Features
import qualified Oracle.Examples.Synth.ISPInfo as ISPInfo
import qualified Oracle.Examples.Synth.ISP as ISP
import qualified Oracle.Examples.Synth.Specs.ESpec as ESpec
import qualified Oracle.Examples.Synth.Basic as Synth
import qualified Oracle.Examples.Synth.Ints2Int as Synth
import qualified Oracle.Examples.Synth.DecisionTree as Synth
import qualified Oracle.Examples.Synth.SynthContext as SynthContext

import Data.Maybe (fromJust)
import GHC.Exts (toList)
import Test.Hspec
import Control.Monad (guard, replicateM)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (get, gets, modify, execStateT)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import System.Random
import qualified Data.List as List

sampleInt :: Int -> Int -> IO Int
sampleInt low high = getStdRandom $ randomR (low, high)

sampleBool :: IO Bool
sampleBool = getStdRandom $ randomR (False, True)

data FeaturesSpec = FeaturesSpec {
  _info   :: ISPInfo,
  _nBools :: Int,
  _nInts  :: Int
  }

type Inputs = (Features Bool, Features Int)

sampleFeatures :: FeaturesSpec -> IO Inputs
sampleFeatures (FeaturesSpec (ISPInfo nTrain nTest) nBools nInts) = do
  bools <- flip mapM [1..nTrain] $ \i -> do
    train <- replicateM nTrain sampleBool
    test  <- replicateM nTrain sampleBool
    pure ("bool" ++ show i, ISP train test)
  ints <- flip mapM [1..nTrain] $ \i -> do
    train <- replicateM nTrain $ sampleInt 0 100
    test  <- replicateM nTrain $ sampleInt 0 100
    pure ("int" ++ show i, ISP train test)
  pure (Features bools, Features ints)

type FeatureIdx = Int

data ArithProg = ArithId FeatureIdx
               | ArithConst Int
               | Add FeatureIdx ArithProg
               | Mul FeatureIdx ArithProg
               | Div1 FeatureIdx ArithProg
               | Div2 ArithProg FeatureIdx
               deriving (Eq, Ord, Show)

mkArithProgs :: Int -> Seq ArithProg
mkArithProgs nIntFeatures = runIdentity $ do
  let maxResults = 100000
  let fuel       = 1000000
  let opts       = BruteForceOptions maxResults fuel BreadthFirst
  results <- bruteForceSearch opts enum
  pure $ fmap Result.value results

  where
    enum :: SearchT Identity ArithProg
    enum = choiceN "" [
      ("", ArithId <$> chooseFeature),
      ("", ArithConst <$> chooseConst),
      ("", Add  <$> chooseFeature <*> enum),
      ("", Mul  <$> chooseFeature <*> enum),
      ("", Div1 <$> chooseFeature <*> enum),
      ("", Div2 <$> enum <*> chooseFeature)
      ]

    chooseFeature :: SearchT Identity FeatureIdx
    chooseFeature = oneOf "" [0..(nIntFeatures - 1)]

    chooseConst :: SearchT Identity Int
    chooseConst = oneOf "" [1, 2, 3, 4, 5, 11, 31, 100]

evalArithProg :: ArithProg -> Features Int -> Maybe (ISP Int)
evalArithProg prog features = case prog of
  ArithId i    -> pure $ Features.getISP i features
  ArithConst k -> pure $ ISP.replicate (Features.getInfo features) k
  Add i prog -> do
    let x = Features.getISP i features
    y <- evalArithProg prog features
    pure $ fmap (\(x, y) -> x + y) (ISP.zip x y)
  Mul i prog -> do
    let x = Features.getISP i features
    y <- evalArithProg prog features
    pure $ fmap (\(x, y) -> x * y) (ISP.zip x y)
  Div1 i prog -> do
    let x = Features.getISP i features
    y <- evalArithProg prog features
    guard $ all (/= 0) y && all ((==0) . uncurry mod) (ISP.zip x y)
    pure $ fmap (\(x, y) -> div x y) (ISP.zip x y)
  Div2 prog i -> do
    x <- evalArithProg prog features
    let y = Features.getISP i features
    guard $ all (/= 0) y && all ((==0) . uncurry mod) (ISP.zip x y)
    pure $ fmap (\(x, y) -> div x y) (ISP.zip x y)

data DTree = Leaf ArithProg
           | Node FeatureIdx DTree DTree
           deriving (Eq, Ord, Show)

mkDTrees :: Int -> Int -> Seq DTree
mkDTrees nBoolFeatures nIntFeatures = runIdentity $ do
  let maxResults = 100000
  let fuel       = 1000000
  let opts       = BruteForceOptions maxResults fuel BreadthFirst
  results <- bruteForceSearch opts enum
  pure $ fmap Result.value results

  where
    enum :: SearchT Identity DTree
    enum = choiceN "" [
      ("", Leaf <$> chooseArithProg),
      ("", Node <$> chooseFeature <*> enum <*> enum)
      ]

    chooseArithProg :: SearchT Identity ArithProg
    chooseArithProg = Seq.index arithProgs <$> oneOf "" [0..(Seq.length arithProgs - 1)]

    chooseFeature :: SearchT Identity FeatureIdx
    chooseFeature = oneOf "" [0..(nBoolFeatures - 1)]

    arithProgs = mkArithProgs nIntFeatures

evalDTree :: DTree -> Inputs -> Maybe (ISP Int)
evalDTree dtree (bools, ints) = guard (not . Features.isEmpty $ bools) *> case dtree of
  Leaf prog -> evalArithProg prog ints
  Node i dtree1 dtree2 -> do
    let b = Features.getISP i bools
    let (infoTrue, infoFalse) = ISP.partitionInfosOn b
    guard $ all okInfo [infoTrue, infoFalse]
    let (boolsTrue, boolsFalse) = SynthContext.partitionOn b bools
    let (intsTrue,  intsFalse ) = SynthContext.partitionOn b ints
    trueGuesses  <- evalDTree dtree1 (boolsTrue,  intsTrue)
    falseGuesses <- evalDTree dtree2 (boolsFalse, intsFalse)
    pure $ ISP.unpartitionOn b trueGuesses falseGuesses

  where
    okInfo (ISPInfo nTrain _) = nTrain > 0

{-
TODO: depending on a flag, either:
1. write data to a file, to be batched trained
2. try solve with universal oracle
-}
main :: IO ()
main = do
  putStrLn "[synth-exe]"
