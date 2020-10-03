{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Oracle.Search.Task where

import Oracle.Control.Monad.Search (SearchT)
import Oracle.Search.Trace (Trace)

data Task m a = Task {
  continuation :: SearchT m a,
  trace        :: Trace
  }
