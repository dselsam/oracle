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
import Oracle.Control.Monad.Search

import Oracle.Examples.Synth.TTS (TTS, ForTrain, ForTest)
import qualified Oracle.Examples.Synth.TTS as TTS

import Oracle.Examples.Synth.Specs.Spec (Spec, SynthFn, SynthFn1)
import qualified Oracle.Examples.Synth.Specs.Spec as Spec

import Oracle.Examples.Synth.Specs.ESpec (ESpec(ESpec))
import qualified Oracle.Examples.Synth.Specs.ESpec as ESpec

import qualified Oracle.Examples.Synth.Basic as Synth

import Oracle.Examples.Synth.Features (Features)
import qualified Oracle.Examples.Synth.Features as Features

import Control.Monad (when, guard)

ints2int :: (Monad m) => Int -> SynthFn m ESpec (Features Int) Int
ints2int maxDepth spec@(ESpec _ xs labels) = synthInt maxDepth spec
  where
    synthInt 0    spec = basecase (0 :: Int) spec
    synthInt fuel spec = choice (snapshot "synthInt" fuel spec) [
      ("basecase", basecase fuel spec),
      ("backup",   do
          x <- oneOf (snapshot "feature" fuel spec) $ Features.choices xs
          let specWithArg = spec { ESpec.ctx = (x, xs) }
          (newSpec, reconstruct) <- choice (snapshot "backup" fuel spec) [
            ("add",  backupAdd  specWithArg),
            ("mul",  backupMul  specWithArg),
            ("div1", backupDiv1 specWithArg),
            ("div2", backupDiv2 specWithArg)
            ]
          guesses <- synthInt (fuel - 1) newSpec
          liftO $ reconstruct guesses)
      ]

    basecase fuel spec = choice (snapshot "basecase" fuel spec) [
      ("identity", do
          x <- oneOf (snapshot "basecase-identity" fuel spec) $ Features.choices xs
          Synth.identity $ spec { ESpec.ctx = x }),
      ("constant", do
          -- TODO: this is unnecessary, but without it, `constant` will be more shallow than `identity`
          -- Possible fixes: heuristics/explicit-costs/scoping
          _ <- oneOf "bump-depth" $ [("bump-depth", ())]
          Synth.constant spec)
      ]

    snapshot name fuel spec = Attrs "ints2int" [
      ("choice", toEmbeddable name),
      ("fuel",   toEmbeddable fuel),
      ("spec",   toEmbeddable spec)
      ]


backupAdd :: (Monad m) => SynthFn1 m ESpec (TTS Int, ctx) Int ESpec ctx Int
backupAdd spec@(ESpec info (xs, ctx) labels) = do
  -- y = x + ?k
  let newLabels :: ForTrain Int = map (\(x, y) -> y - x) (zip (TTS.train xs) labels)
  let reconstruct guesses = pure $ TTS.map (uncurry (+)) (TTS.zip xs guesses)
  pure (ESpec info ctx newLabels, reconstruct)

backupMul :: (Monad m) => SynthFn1 m ESpec (TTS Int, ctx) Int ESpec ctx Int
backupMul spec@(ESpec info (xs, ctx) labels) = do
  -- y = x * ?k
  newLabels :: ForTrain Int <- flip mapM (zip (TTS.train xs) labels) $ \(x, y) -> do
    guard $ x /= 0
    guard $ y `rem` x == 0
    pure $ y `div` x
  guard . all (/=0) $ TTS.test xs
  let reconstruct guesses = pure $ TTS.map (uncurry (*)) (TTS.zip xs guesses)
  pure (ESpec info ctx newLabels, reconstruct)

backupDiv1 :: (Monad m) => SynthFn1 m ESpec (TTS Int, ctx) Int ESpec ctx Int
backupDiv1 spec@(ESpec info (xs, ctx) labels) = do
  -- y = ?k / x
  newLabels :: ForTrain Int <- flip mapM (zip (TTS.train xs) labels) $ \(x, y) -> do
    guard $ x /= 0
    pure  $ x * y
  guard . all (/= 0) $ TTS.test xs
  let reconstruct guesses = pure $ TTS.map (uncurry div) (TTS.zip guesses xs)
  pure (ESpec info ctx newLabels, reconstruct)

backupDiv2 :: (Monad m) => SynthFn1 m ESpec (TTS Int, ctx) Int ESpec ctx Int
backupDiv2 spec@(ESpec info (xs, ctx) labels) = do
  -- y = x / ?k
  newLabels :: ForTrain Int <- flip mapM (zip (TTS.train xs) labels) $ \(x, y) -> do
    guard $ y /= 0
    guard $ x `rem` y == 0
    pure  $ x `div` y
  let reconstruct guesses = do { guard (TTS.all (/= 0) guesses); pure $ TTS.map (uncurry div) (TTS.zip xs guesses) }
  pure (ESpec info ctx newLabels, reconstruct)
