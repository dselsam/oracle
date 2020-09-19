{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Trivial specifications.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Oracle.Examples.Synth.Specs.TSpec where

import Oracle.Examples.Synth.ISP (ISP(ISP), ForTrain, ForTest)
import Oracle.Examples.Synth.ISPInfo
import qualified Oracle.Examples.Synth.ISP as ISP
import Oracle.Examples.Synth.Specs.Spec

data TSpec ctx a = TSpec {
  info        :: ISPInfo,
  ctx         :: ctx
  } deriving (Show)

instance Spec TSpec ctx a where
  info  (TSpec info   _     )         = info
  ctx   (TSpec _      inputs)         = inputs
  check (TSpec _      _     ) guesses = True
