{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Test.Oracle.Examples.Synth.Ints2Int where

import Oracle.SearchT
import Oracle.Search.BruteForce
import qualified Oracle.Search.Result as Result

import Oracle.Examples.Synth
import qualified Oracle.Examples.Synth.TTSInfo as TTSInfo
import qualified Oracle.Examples.Synth.TTS as TTS
import qualified Oracle.Examples.Synth.Specs.ESpec as ESpec
import qualified Oracle.Examples.Synth.Basic as Synth
import qualified Oracle.Examples.Synth.Ints2Int as Synth

import GHC.Exts (toList)
import Test.Hspec
import Control.Monad (guard)
import Control.Monad.Identity (Identity, runIdentity)

find1 :: SearchT Identity (TTS a) -> [ForTest a]
find1 f = let opts = BruteForceOptions 1 10000 BreadthFirst
              results = runIdentity $ bruteForceSearch opts f
          in
            map (TTS.test . Result.value) (toList results)

testBasic = describe "testBasic" $ do
  let phi1     = ("ascending",  TTS [1 :: Int, 2, 3] [4, 5])
  let phi2     = ("descending", TTS [3, 2, 1] [5, 4])
  let phi3     = ("random",     TTS [1, 7, 3] [2, 6])
  let phi4     = ("multiples",  TTS [10, 30, 20] [70, 60])
  let phi5     = ("mod3s",      TTS [1, 1, 2] [0, 1])
  let maxDepth = 2
  let info     = TTSInfo 3 2
  let inputs   = Features [phi1, phi2, phi3, phi4, phi5]

  it "const" $ do
    find1 (Synth.ints2int maxDepth $ ESpec info inputs [1, 1, 1]) `shouldBe` [[1, 1]]

  it "input" $ do
    find1 (Synth.ints2int maxDepth $ ESpec info inputs [3, 2, 1]) `shouldBe` [[5, 4]]

  it "input+const" $ do
    find1 (Synth.ints2int maxDepth $ ESpec info inputs [33, 32, 31]) `shouldBe` [[35, 34]]

  it "input+input" $ do
    find1 (Synth.ints2int maxDepth $ ESpec info inputs [2, 9, 6]) `shouldBe` [[6, 11]]

  it "input-const" $ do
    find1 (Synth.ints2int maxDepth $ ESpec info inputs [-7, -8, -9])  `shouldBe` [[-5, -6]]

  it "input*const" $ do
    find1 (Synth.ints2int maxDepth $ ESpec info inputs [30, 20, 10]) `shouldBe` [[50, 40]]

  it "const/input" $ do
    find1 (Synth.ints2int maxDepth $ ESpec info inputs [40, 60, 120]) `shouldBe` [[24, 30]]

  it "input/const" $ do
    find1 (Synth.ints2int maxDepth $ ESpec info inputs [1, 3, 2]) `shouldBe` [[7, 6]]

  it "input%3" $ do
    find1 (Synth.ints2int maxDepth $ ESpec info inputs [1, 1, 2]) `shouldBe` [[0, 1]]

tests = describe "Test.Oracle.Examples.Synth.Ints2Int" $ do
  testBasic
