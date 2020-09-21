{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

{-# LANGUAGE ScopedTypeVariables #-}
module Test.Oracle.Search.BestFirst where

import Oracle.SearchT
import Oracle.Search.BestFirst
import qualified Oracle.Search.Result as Result
import qualified Oracle.Neural.Oracle as Oracle
import qualified Oracle.Neural.Result as Neural

import GHC.Exts (toList)
import Test.Hspec
import Control.Monad (guard)
import Control.Monad.Identity (Identity, runIdentity)

search :: Int -> SearchT IO a -> IO [a]
search maxResults f = do
  let opts = BestFirstOptions maxResults 10000
  let oracle cp = do
        Neural.Result pi _ <- Oracle.query Oracle.dummy cp
        pure pi
  results <- bestFirstSearch opts oracle f
  pure $ map Result.value (toList results)

testSimple = describe "testSimple" $ do
  let odds = do
        n1 :: Int <- oneOf () [0..10]
        if mod n1 2 == 1 then pure (n1, 0) else do
          n2 :: Int <- oneOf () [0..10]
          guard $ mod (n1 + n2) 2 == 1
          pure (n1, n2)

  result1 <- runIO (search 2 odds)
  it "uniform best-first should be breadth-first [(1, 0), (3, 0)]" $ do
    result1 `shouldBe` [(1, 0), (3, 0)]

tests = describe "Test.Oracle.Search.BestFirst" $ do
  testSimple
