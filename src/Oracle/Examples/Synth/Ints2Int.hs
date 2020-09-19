{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Synthesizing simple arithmetic formulae, `[Int] -> Int`.
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

data Ints2IntOptions = Ints2IntOptions {
  maxDepth :: Int
  } deriving (Eq, Ord, Show)

ints2Int :: (Monad m) => Ints2IntOptions -> SynthFn m ESpec (Features Int) Int
ints2Int opts spec@(ESpec _ xs labels) = synthInt (maxDepth opts) spec
  where
    synthInt 0    spec = basecase 0 spec
    synthInt fuel spec = choiceN "synthInt" [
      ("basecase", basecase fuel spec),
      ("backup",   do
          x <- oneOfN "arg" $ Features.choices xs
          let specWithArg = spec { ESpec.ctx = (x, xs) }
          (newSpec, reconstruct) <- choiceN "backup" [
            ("add",  backupAdd  specWithArg),
            ("mul",  backupMul  specWithArg),
            ("div1", backupDiv1 specWithArg),
            ("div2", backupDiv2 specWithArg)
            ]
          guesses <- synthInt (fuel - 1) newSpec
          liftO $ reconstruct guesses)
      ]

    basecase fuel spec = choiceN "leaf" [
      ("identity", do
          x <- oneOfN "identity" $ Features.choices xs
          neg <- oneOfN "neg" [("pos", id), ("neg", \x -> -x)]
          Synth.identity $ spec { ESpec.ctx = (ISP.map neg x) }),
      ("constant", Synth.constant spec),
      ("modulo", do
          x <- oneOfN "identity" $ Features.choices xs
          k <- oneOfN "modulus" [("2", 2), ("3", 3)]
          leafModulo k $ spec { ESpec.ctx = x })
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

leafModulo :: (Monad m) => Int -> SynthFn m ESpec (ISP Int) Int
leafModulo k spec@(ESpec info xs labels) = do
  -- y = x % k
  let guesses = ISP.map (flip mod k) xs
  guard . all (uncurry (==)) $ zip (ISP.train guesses) labels
  pure guesses
