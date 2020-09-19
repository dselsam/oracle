{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Simple inductive synthesis library.
-}

module Oracle.Examples.Synth (
  ISPInfo(ISPInfo),
  ISP(ISP), ForTrain, ForTest,
  SynthContext,
  Features(Features),
  Spec,
  ESpec(ESpec)
  ) where

import Oracle.Examples.Synth.ISPInfo (ISPInfo(ISPInfo))
import Oracle.Examples.Synth.ISP (ISP(ISP), ForTrain, ForTest)
import Oracle.Examples.Synth.SynthContext (SynthContext)
import Oracle.Examples.Synth.Features (Features(Features))
import Oracle.Examples.Synth.Specs.Spec (Spec)
import Oracle.Examples.Synth.Specs.ESpec (ESpec(ESpec))
