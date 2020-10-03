{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

-}
module Oracle.Neural.Query where

import Oracle.Control.Monad.Search (ChoicePoint(ChoicePoint))
import Oracle.Data.Embeddable

import qualified Data.Vector as Vector

import GHC.Exts (toList)

import Control.Monad.Trans (MonadIO)

import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Query = Query {
  snapshot :: Embeddable,
  choices  :: Vector Embeddable
  } deriving (Eq, Ord, Show)

fromChoicePoint :: ChoicePoint m a -> Query
fromChoicePoint (ChoicePoint snapshot choices) = Query {
  snapshot = snapshot,
  choices  = Vector.fromList . toList $ fmap fst choices
  }
