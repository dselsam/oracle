{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Synthesizing simple arithmetic formulae `[Int] -> Int` with deductive backpropagation.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}
module Oracle.Examples.Synth.Ints2Int where

import Oracle.Data.Embeddable
import Oracle.SearchT

import Oracle.Examples.Synth.ISP (ISP, ForTrain, ForTest)
import qualified Oracle.Examples.Synth.ISP as ISP

import Oracle.Examples.Synth.Specs.Spec (Spec, SynthFn, SynthFn1)
import qualified Oracle.Examples.Synth.Specs.Spec as Spec

import Oracle.Examples.Synth.Specs.ESpec (ESpec(ESpec))
import qualified Oracle.Examples.Synth.Specs.ESpec as ESpec

import qualified Oracle.Examples.Synth.Basic as Synth

import Oracle.Examples.Synth.Features (Features)
import qualified Oracle.Examples.Synth.Features as Features

import Control.Monad (when, guard)

ints2Int :: (Monad m) => Int -> SynthFn m ESpec (Features Int) Int
ints2Int maxDepth spec@(ESpec _ xs labels) = synthInt maxDepth spec
  where
    synthInt 0    spec = basecase spec
    synthInt fuel spec = choiceN "synthInt" [
      ("basecase", basecase spec),
      ("backup",   do
          x <- oneOfN (snapshot "feature" fuel spec) $ Features.choices xs
          let specWithArg = spec { ESpec.ctx = (x, xs) }
          (newSpec, reconstruct) <- choiceN (snapshot "backup" fuel spec) [
            ("add",  backupAdd  specWithArg),
            ("mul",  backupMul  specWithArg),
            ("div1", backupDiv1 specWithArg),
            ("div2", backupDiv2 specWithArg)
            ]
          guesses <- synthInt (fuel - 1) newSpec
          liftO $ reconstruct guesses)
      ]

    basecase spec = choiceN "leaf" [
      ("identity", do
          x <- oneOfN (snapshot "basecase-identity" (0 :: Int) spec) $ Features.choices xs
          Synth.identity $ spec { ESpec.ctx = x }),
      ("constant", do
          -- TODO: this is unnecessary, but without it, `constant` will be more shallow than `identity`
          -- Possible fixes: heuristics/explicit-costs/scoping
          _ <- oneOfN "dump-depth" $ [("bump-depth", ())]
          Synth.constant spec)
      ]

    snapshot name fuel spec = Attrs "ints2int" [
      ("choice", toEmbeddable name),
      ("fuel",   toEmbeddable fuel),
      ("spec",   toEmbeddable spec)
      ]


backupAdd :: (Monad m) => SynthFn1 m ESpec (ISP Int, ctx) Int ESpec ctx Int
backupAdd spec@(ESpec info (xs, ctx) labels) = do
  -- y = x + ?k
  let newLabels :: ForTrain Int = map (\(x, y) -> y - x) (zip (ISP.train xs) labels)
  let reconstruct guesses = pure $ ISP.map (uncurry (+)) (ISP.zip xs guesses)
  pure (ESpec info ctx newLabels, reconstruct)

backupMul :: (Monad m) => SynthFn1 m ESpec (ISP Int, ctx) Int ESpec ctx Int
backupMul spec@(ESpec info (xs, ctx) labels) = do
  -- y = x * ?k
  newLabels :: ForTrain Int <- flip mapM (zip (ISP.train xs) labels) $ \(x, y) -> do
    guard $ x /= 0
    guard $ y `rem` x == 0
    pure $ y `div` x
  guard . all (/=0) $ ISP.test xs
  let reconstruct guesses = pure $ ISP.map (uncurry (*)) (ISP.zip xs guesses)
  pure (ESpec info ctx newLabels, reconstruct)

backupDiv1 :: (Monad m) => SynthFn1 m ESpec (ISP Int, ctx) Int ESpec ctx Int
backupDiv1 spec@(ESpec info (xs, ctx) labels) = do
  -- y = ?k / x
  newLabels :: ForTrain Int <- flip mapM (zip (ISP.train xs) labels) $ \(x, y) -> do
    guard $ x /= 0
    pure  $ x * y
  guard . all (/= 0) $ ISP.test xs
  let reconstruct guesses = pure $ ISP.map (uncurry div) (ISP.zip guesses xs)
  pure (ESpec info ctx newLabels, reconstruct)

backupDiv2 :: (Monad m) => SynthFn1 m ESpec (ISP Int, ctx) Int ESpec ctx Int
backupDiv2 spec@(ESpec info (xs, ctx) labels) = do
  -- y = x / ?k
  newLabels :: ForTrain Int <- flip mapM (zip (ISP.train xs) labels) $ \(x, y) -> do
    guard $ y /= 0
    guard $ x `rem` y == 0
    pure  $ x `div` y
  let reconstruct guesses = do { guard (ISP.all (/= 0) guesses); pure $ ISP.map (uncurry div) (ISP.zip xs guesses) }
  pure (ESpec info ctx newLabels, reconstruct)
