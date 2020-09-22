{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Trivial specifications.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Oracle.Examples.Synth.Specs.TSpec where

import Oracle.Examples.Synth.TTS (TTS(TTS), ForTrain, ForTest)
import Oracle.Examples.Synth.TTSInfo
import qualified Oracle.Examples.Synth.TTS as TTS
import Oracle.Examples.Synth.Specs.Spec

data TSpec ctx a = TSpec {
  info        :: TTSInfo,
  ctx         :: ctx
  } deriving (Show)

instance Spec TSpec ctx a where
  info  (TSpec info   _     )         = info
  ctx   (TSpec _      inputs)         = inputs
  check (TSpec _      _     ) guesses = True
