{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

This is a placeholder file, until we decide how best to control the learning.
The two main options are:

1. Wait until the supposedly-imminent release of a Haskell Torch library:

- https://github.com/hasktorch/hasktorch
- https://github.com/abarbu/haskell-torch

2. Communicate with Python over a socket using protobufs.
-}
module Oracle.Neural where

import Oracle.Data.Embeddable
import Oracle.SearchT hiding (snapshot, choices)

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import GHC.Exts (toList)

import Control.Monad.Trans (MonadIO)

import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Result = Result {
  policyHead :: Vector Float,
  valueHead  :: Maybe Float
  } deriving (Eq, Ord, Show)

queryUniversalOracle :: (MonadIO m2) => ChoicePoint m a -> m2 Result
queryUniversalOracle cp = pure $ Result {
  policyHead = fmap (\_->x) (choices query),
  valueHead  = Just 0.5
  }
  where
    query = cp2query cp
    x = 1.0 / (fromIntegral $ Vector.length $ choices query)

trainUniversalOracle :: (MonadIO m2) => ChoicePoint m a -> Result -> m2 ()
trainUniversalOracle _ _ = pure ()

data Query = Query {
  snapshot :: Embeddable,
  choices  :: Vector Embeddable
  } deriving (Eq, Ord, Show)

cp2query :: ChoicePoint m a -> Query
cp2query (ChoicePoint snapshot choices) = Query {
  snapshot = snapshot,
  choices  = Vector.fromList . toList $ fmap fst choices
  }
