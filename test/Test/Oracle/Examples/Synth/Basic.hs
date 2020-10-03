{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Test.Oracle.Examples.Synth.Basic where

import Oracle.Control.Monad.Search
import Oracle.Search.BruteForce
import qualified Oracle.Search.Result as Result

import Oracle.Examples.Synth
import qualified Oracle.Examples.Synth.TTSInfo as TTSInfo
import qualified Oracle.Examples.Synth.TTS as TTS
import qualified Oracle.Examples.Synth.Specs.ESpec as ESpec
import qualified Oracle.Examples.Synth.Basic as Synth

import GHC.Exts (toList)
import Test.Hspec
import Control.Monad (guard)
import Control.Monad.Identity (Identity, runIdentity)

find1 :: SearchT Identity (TTS a) -> [ForTest a]
find1 f = let opts = BruteForceOptions 1 10000 BreadthFirst
              results = runIdentity $ bruteForceSearch opts f
          in
            map (TTS.test . Result.value) (toList results)

testIdentity = describe "testIdentity" $ do
  let feature = TTS {
        TTS.train = [1, 2, 3, 4, 5],
        TTS.test  = [6, 7]
        }

  let especId = ESpec {
        ESpec.info = TTSInfo 5 2,
        ESpec.ctx  = feature,
        ESpec.labels = [1, 2, 3, 4, 5]
        }

  let especNotId = ESpec {
        ESpec.info = TTSInfo 5 2,
        ESpec.ctx  = feature,
        ESpec.labels = [1, 2, 3, 4, 6]
        }

  it "should synth identity for especId" $ do
    find1 (Synth.identity especId) `shouldBe` [[6, 7]]

  it "should fail for especNotId" $ do
    find1 (Synth.identity especNotId) `shouldBe` []

testIfEqConst = describe "testIfEqConst" $ do
  let feature = TTS {
        TTS.train = [1, 2, 3, 2, 2],
        TTS.test  = [2, 7]
        }

  let especIfEqConst = ESpec {
        ESpec.info = TTSInfo 5 2,
        ESpec.ctx  = feature,
        ESpec.labels = [False, True, False, True, True]
        }

  let especNotIfEqConst = ESpec {
        ESpec.info = TTSInfo 5 2,
        ESpec.ctx  = feature,
        ESpec.labels = [False, True, True, True, True]
        }

  it "should synth `x==2` for especIfEqConst" $ do
    find1 (Synth.ifEqConst especIfEqConst) `shouldBe` [[True, False]]

  it "should fail for especNotIfEqConst" $ do
    find1 (Synth.ifEqConst especNotIfEqConst) `shouldBe` []

tests = describe "Test.Oracle.Examples.Synth.Basic" $ do
  testIdentity
  testIfEqConst
