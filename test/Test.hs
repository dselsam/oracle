{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Main where

import Test.Hspec
import qualified Test.Oracle.Search.BruteForce

main :: IO ()
main = hspec $ do
  Test.Oracle.Search.BruteForce.tests
