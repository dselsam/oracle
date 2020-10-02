{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Test.Oracle.Examples.Synth.DecisionTree where

import Oracle.Control.Monad.Search
import Oracle.Search.BruteForce
import qualified Oracle.Search.Result as Result

import Oracle.Examples.Synth
import qualified Oracle.Examples.Synth.TTSInfo as TTSInfo
import qualified Oracle.Examples.Synth.TTS as TTS
import qualified Oracle.Examples.Synth.Specs.ESpec as ESpec
import qualified Oracle.Examples.Synth.Basic as Synth
import qualified Oracle.Examples.Synth.Ints2Int as Synth
import qualified Oracle.Examples.Synth.DecisionTree as Synth

import GHC.Exts (toList)
import Test.Hspec
import Control.Monad (guard)
import Control.Monad.Identity (Identity, runIdentity)

find1 :: SearchT Identity (TTS a) -> [ForTest a]
find1 f = let opts = BruteForceOptions 1 10000 BreadthFirst
              results = runIdentity $ bruteForceSearch opts f
          in
            map (TTS.test . Result.value) (toList results)

testNaiveBasic = describe "testNaiveBasic" $ do
  let bs1     = ("bools1", TTS [True,  True,  False, False] [False,  True])
  let bs2     = ("bools2", TTS [False, True,  True,  False] [True,  False])

  let phi1     = ("ascending",  TTS [1,   2,  3,  4] [ 5,  6])
  let phi2     = ("random",     TTS [11, 51, 41, 31] [29, 61])
  let phi3     = ("primes",     TTS [2,   3,  5,  7] [11, 13])
  let phi4     = ("multiples",  TTS [10, 30, 20, 40] [70, 60])

  let info      = TTSInfo 4 2
  let bools     = Features [bs1, bs2]
  let ints      = Features [phi1, phi2, phi3, phi4]

  let synth     = \d labels -> find1 $ Synth.decisionTreeNaive d (Synth.ints2int 0) $ ESpec info (bools, ints) labels

  it "no-split" $ do
    -- ascending
    synth 1 [1, 2, 3, 4] `shouldBe` [[5, 6]]

  it "1-split" $ do
    -- if b2 then multiples else primes
    synth 1 [2, 30, 20, 7] `shouldBe` [[70, 13]]

  it "2-split" $ do
    -- if b2 then multiples else (if b1 then random else ascending)
    synth 2 [11, 30, 20, 4] `shouldBe` [[70, 61]]

tests = describe "Test.Oracle.Examples.Synth.DecisionTree" $ do
  testNaiveBasic
