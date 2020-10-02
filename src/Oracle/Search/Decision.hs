{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Oracle.Search.Decision where

import Oracle.Data.Embeddable

import Data.Vector (Vector)

data Decision = Decision {
  snapshot  :: Embeddable,
  choices   :: Vector Embeddable,
  choiceIdx :: Int,
  score     :: Float
  } deriving (Eq, Ord, Show)
