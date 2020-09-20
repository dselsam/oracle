{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Immensely naive decision tree synthesizer.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Oracle.Examples.Synth.DecisionTree where

import Oracle.Data.Embeddable
import Oracle.SearchT

import Oracle.Examples.Synth.Features (Features(Features))
import qualified Oracle.Examples.Synth.Features as Features

import Oracle.Examples.Synth.SynthContext (SynthContext)
import qualified Oracle.Examples.Synth.SynthContext as SynthContext

import Oracle.Examples.Synth.ISPInfo (ISPInfo(ISPInfo))
import qualified Oracle.Examples.Synth.ISPInfo as ISPInfo

import Oracle.Examples.Synth.ISP (ISP(ISP))
import qualified Oracle.Examples.Synth.ISP as ISP

import Oracle.Examples.Synth.Specs.Spec (Spec, SynthFn)
import qualified Oracle.Examples.Synth.Specs.Spec as Spec

import Oracle.Examples.Synth.Specs.ESpec (ESpec(ESpec))
import qualified Oracle.Examples.Synth.Specs.ESpec as ESpec

import Oracle.Util.Misc (flipl)
import qualified Oracle.Util.List as List

import Debug.Trace (traceM)
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO)
import Data.Foldable (foldlM)
import Data.Map (Map)
import qualified Data.Map as Map

decisionTreeNaive :: (Monad m, SynthContext ctx, HasToEmbeddable ctx, HasToEmbeddable b) => Int -> SynthFn m ESpec ctx b -> SynthFn m ESpec (Features Bool, ctx) b
decisionTreeNaive fuel synthLeaf spec
  | fuel == 0 = basecase spec
  | otherwise = core fuel spec

  where
    basecase (ESpec info (_, ctx) labels)
      | ISPInfo.isEmpty info = deadend ""
      | otherwise            = synthLeaf $ spec { ESpec.ctx = ctx }

    core fuel spec@(ESpec info (bs, ctx) labels) = choiceN (snapshot "leafVsNode" fuel spec) $ [
      ("leaf", basecase spec),
      ("node", do
          b <- oneOfN (snapshot "feature" fuel spec) $ Features.choices bs
          let (infoTrue, infoFalse)     = ISP.partitionInfosOn b
          guard $ all okInfo [infoTrue, infoFalse]
          let (bsTrue, bsFalse)         = SynthContext.partitionOn b bs
          let (ctxTrue, ctxFalse)       = SynthContext.partitionOn b ctx
          let (labelsTrue, labelsFalse) = List.partitionOn (ISP.train b) labels
          trueGuesses  <- decisionTreeNaive (fuel-1) synthLeaf $ ESpec infoTrue  (bsTrue,  ctxTrue)  labelsTrue
          falseGuesses <- decisionTreeNaive (fuel-1) synthLeaf $ ESpec infoFalse (bsFalse, ctxFalse) labelsFalse
          pure $ ISP.unpartitionOn b trueGuesses falseGuesses)
      ]

    snapshot name fuel spec = Attrs "decisionTreeNaive" [
      ("choice", toEmbeddable name),
      ("fuel",   toEmbeddable fuel),
      ("spec",   toEmbeddable spec)
      ]

    okInfo (ISPInfo nTrain _) = nTrain > 0
