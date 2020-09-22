{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Metadata for train-test split (TTS).
-}

module Oracle.Examples.Synth.TTSInfo where

import Oracle.Data.Embeddable

data TTSInfo = TTSInfo {
  nTrain :: Int,
  nTest  :: Int
  } deriving (Eq, Ord, Show)

isEmpty :: TTSInfo -> Bool
isEmpty info = nTrain info == 0 && nTest info == 0

instance HasToEmbeddable TTSInfo where
  toEmbeddable (TTSInfo nTrain nTest) = ERecord "TTSInfo" [
    ("nTrain",   toEmbeddable nTrain),
    ("nTest",    toEmbeddable nTest)
    ]
