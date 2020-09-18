{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Oracle.Grid where

import Data.Map (Map)
import qualified Data.Map as Map

-- TODO: very naive
data Grid a = Grid {
  nRows    :: Int,
  nCols    :: Int,
  idxs2val :: Map (Int, Int) a
  } deriving (Eq, Ord, Show)

instance Functor Grid where
  fmap f g@(Grid _ _ m) = g { idxs2val = Map.map f m }
