{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Basic inductive synthesis building blocks.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Oracle.Examples.Synth.Basic where

import Oracle.Data.Embeddable
import Oracle.SearchT

import Oracle.Examples.Synth.Features (Features(Features))

import Oracle.Examples.Synth.TTS (TTS)
import qualified Oracle.Examples.Synth.TTS as TTS

import Oracle.Examples.Synth.Specs.Spec (Spec, SynthFn)
import qualified Oracle.Examples.Synth.Specs.Spec as Spec

import Oracle.Examples.Synth.Specs.ESpec (ESpec(ESpec))
import qualified Oracle.Examples.Synth.Specs.ESpec as ESpec

import Oracle.Util.Misc (flipl)
import qualified Oracle.Util.List as List

import Control.Monad (guard)
import Data.Foldable (foldlM)
import Data.Map (Map)
import qualified Data.Map as Map

-- Checks:  the inputs are exactly the outputs
-- Guesses: input
identity :: (Monad m, Spec spec (TTS a) a, Eq a) => SynthFn m spec (TTS a) a
identity spec = do
  let x = Spec.ctx spec
  guard $ Spec.check spec x
  pure x

-- Checks:  every input that maps to true is the same
-- Guesses: if input == <special> then True else False
ifEqConst :: (Monad m, Eq a) => SynthFn m ESpec (TTS a) Bool
ifEqConst (ESpec _ inputs labels) = do
  let trues  = map fst . filter snd $ zip (TTS.train inputs) labels
  let falses = map fst . filter (not . snd) $ zip (TTS.train inputs) labels
  guard . not . null   $ trues
  guard . List.allSame $ trues
  let special = head trues
  guard . not $ special `elem` falses
  pure $ TTS.map (==special) inputs

-- Checks:  there is only one output value among all inputs
-- Guesses: <special>
constant :: (Monad m, Eq b) => SynthFn m ESpec ctx b
constant spec = do
  guard . not . null $ ESpec.labels spec
  guard . List.allSame $ ESpec.labels spec
  pure $ TTS.replicate (Spec.info spec) $ head $ ESpec.labels spec

-- Checks:  every input is associated with at most one output
-- Checks:  at least two inputs are repeated
-- Checks:  all test inputs appeared among the train inputs
-- Guesses: the image of the lookup table
lookupTable :: (Monad m, Ord a, Eq b, Ord b) => SynthFn m ESpec (TTS a) b
lookupTable spec@(ESpec _ inputs labels) = do
  lookupTableWithCounts :: Map a (b, Int) <- flipl foldlM (zip (TTS.train inputs) labels) Map.empty $ \acc (k, v) ->
    case Map.lookup k acc of
      Nothing      -> pure $ Map.insert k (v, 1) acc
      Just (v', n) -> do
        guard (v == v')
        pure $ Map.insert k (v, n+1) acc

  guard . (>=2) . length . filter (\(v, c) -> c >= 2) . Map.elems $ lookupTableWithCounts
  guard . List.allDistinct . map fst . Map.elems $ lookupTableWithCounts
  let lookupTable :: Map a b = Map.map fst lookupTableWithCounts
  liftO $ TTS.mapM (flip Map.lookup lookupTable) inputs

focus :: (Monad m) => SynthFn m ESpec (TTS a) b -> SynthFn m ESpec (Features a) b
focus synthEx (ESpec info (Features xs) labels) = do
  -- TODO: need conventions for snapshots/choices
  x <- oneOfN "focus" xs
  synthEx $ ESpec info x labels
