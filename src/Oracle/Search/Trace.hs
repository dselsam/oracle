{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Oracle.Search.Trace where

import Oracle.Search.Decision (Decision)

newtype Trace = Trace {
  decisions :: [Decision]
  } deriving (Eq, Ord, Show)
