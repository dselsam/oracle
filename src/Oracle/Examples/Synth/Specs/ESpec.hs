{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Exact specifications.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Oracle.Examples.Synth.Specs.ESpec where

import Oracle.Data.Embeddable
import Oracle.Examples.Synth.ISP (ISP(ISP), ForTrain, ForTest)
import Oracle.Examples.Synth.ISPInfo
import qualified Oracle.Examples.Synth.ISP as ISP
import Oracle.Examples.Synth.Specs.Spec
import qualified Oracle.Examples.Synth.ISPInfo as ISPInfo

data ESpec ctx a = ESpec {
  info   :: ISPInfo,
  ctx    :: ctx,
  labels :: ForTrain a
  } deriving (Eq, Ord, Show)

instance (Eq a) => Spec ESpec ctx a where
  info  (ESpec info   _      _     )         = info
  ctx   (ESpec _      inputs _     )         = inputs
  check (ESpec _      _      labels) guesses = labels == ISP.train guesses

instance (HasToEmbeddable ctx, HasToEmbeddable a) => HasToEmbeddable (ESpec ctx a) where
  toEmbeddable (ESpec info ctx labels) = ERecord "ESpec" [
    ("info",   toEmbeddable info),
    ("ctx",    toEmbeddable ctx),
    ("labels", toEmbeddable labels)
    ]
