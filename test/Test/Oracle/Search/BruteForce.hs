{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

{-# LANGUAGE ScopedTypeVariables #-}
module Test.Oracle.Search.BruteForce where

import Oracle.SearchT
import Oracle.Search.BruteForce
import GHC.Exts (toList)
import Test.Hspec
import Control.Monad (guard)
import Control.Monad.Identity (Identity, runIdentity)

search :: Int -> SearchAlg -> SearchT Identity a -> [a]
search maxResults searchAlg f =
  let opts = BruteForceOptions maxResults 10000 searchAlg
      results = runIdentity $ bruteForceSearch opts f
  in
    map value (toList results)

testSimple = describe "testSimple" $ do
  let odds = do
        n1 :: Int <- oneOf () [0..10]
        if mod n1 2 == 1 then pure (n1, 0) else do
          n2 :: Int <- oneOf () [0..10]
          guard $ mod (n1 + n2) 2 == 1
          pure (n1, n2)

  it "depth first should give [(0, 1), (0, 3)]" $ do
    search 2 DepthFirst odds `shouldBe` [(0, 1), (0, 3)]

  it "breadth first should give [(0, 1), (0, 3)]" $ do
    search 2 BreadthFirst odds `shouldBe` [(1, 0), (3, 0)]

tests = do
  testSimple
