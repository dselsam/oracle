{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Disjunctive specifications.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Oracle.Examples.Synth.Specs.DSpec where

import Oracle.Data.Embeddable
import Oracle.Control.Monad.Search
import Oracle.Examples.Synth.TTS (TTS(TTS), ForTrain, ForTest)
import Oracle.Examples.Synth.TTSInfo
import qualified Oracle.Examples.Synth.TTS as TTS
import Oracle.Examples.Synth.Specs.ESpec (ESpec(ESpec))
import Oracle.Examples.Synth.Specs.Spec
import qualified Data.List as List

data DSpec ctx a = DSpec {
  info   :: TTSInfo,
  ctx    :: ctx,
  labels :: ForTrain [a]
  } deriving (Eq, Ord, Show)

instance (Eq a) => Spec DSpec ctx a where
  info   (DSpec info      _   _     )      = info
  ctx    (DSpec _         ctx _     )      = ctx
  check  (DSpec _      _   labels) guesses = flip all (zip labels (TTS.train guesses)) $ \(label, guess) -> guess `elem` label

nExactSpecs :: DSpec ctx a -> Int
nExactSpecs (DSpec _ _ labels) = List.product (map length labels)

blast :: (Monad m, HasToEmbeddable a, HasToEmbeddable b) => b -> DSpec ctx a -> SearchT m (ESpec ctx a)
blast snapshot (DSpec info ctx labels) = do
  labels :: ForTrain a <- flip mapM (zip [1..] labels) $ \(i, labels) -> oneOfSelf snapshot labels
  pure $ ESpec info ctx labels
