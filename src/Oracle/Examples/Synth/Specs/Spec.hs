{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Abstract class for specifications.
This abstraction level is rarely used, since most procedures handle only one type of spec.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
module Oracle.Examples.Synth.Specs.Spec where

import Oracle.SearchT
import Oracle.Examples.Synth.TTSInfo (TTSInfo(TTSInfo))
import Oracle.Examples.Synth.TTS (TTS(TTS), ForTrain, ForTest)
import qualified Oracle.Examples.Synth.TTS as TTS

type ReconstructFn a b = TTS a -> Maybe (TTS b)
type SynthFn  m spec ctx a = spec ctx a -> SearchT m (TTS a)
type SynthFn1 m s1 c1 a1 s2 c2 a2 = s1 c1 a1 -> SearchT m (s2 c2 a2, ReconstructFn a2 a1)

class Spec spec ctx a where
  info  :: spec ctx a -> TTSInfo
  ctx   :: spec ctx a -> ctx
  check :: spec ctx a -> TTS a -> Bool
