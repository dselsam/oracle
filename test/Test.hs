{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Main where

import Test.Hspec
import qualified Test.Oracle.Search.BruteForce
import qualified Test.Oracle.Search.BestFirst
import qualified Test.Oracle.Examples.Synth.Basic
import qualified Test.Oracle.Examples.Synth.Ints2Int
import qualified Test.Oracle.Examples.Synth.DecisionTree

main :: IO ()
main = hspec $ do
  Test.Oracle.Search.BruteForce.tests
  Test.Oracle.Search.BestFirst.tests
  Test.Oracle.Examples.Synth.Basic.tests
  Test.Oracle.Examples.Synth.Ints2Int.tests
  Test.Oracle.Examples.Synth.DecisionTree.tests
