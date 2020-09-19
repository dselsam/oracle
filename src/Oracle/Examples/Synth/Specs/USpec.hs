{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Unique specifications, i.e. the constraint that there is a "unique" element for
each set of inputs, so that |countDistinct(guesses)==1| for every input.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Oracle.Examples.Synth.Specs.USpec where

import Oracle.Examples.Synth.ISP (ISP(ISP), ForTrain, ForTest)
import Oracle.Examples.Synth.ISPInfo
import qualified Oracle.Examples.Synth.ISP as ISP
import Oracle.Examples.Synth.Specs.Spec
import qualified Oracle.Util.List as List

data USpec ctx a = USpec {
  info        :: ISPInfo,
  ctx         :: ctx
  } deriving (Show)

instance (Eq a, Ord a) => Spec USpec ctx [a] where
  info  (USpec info   _     )         = info
  ctx   (USpec _      inputs)         = inputs
  check (USpec _      _     ) guesses = ISP.all ((==1) . List.countDistinct id) guesses
