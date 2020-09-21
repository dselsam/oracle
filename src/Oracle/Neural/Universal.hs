{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

-}
module Oracle.Neural.Universal where

import Oracle.Data.Embeddable
import Oracle.SearchT hiding (snapshot, choices)

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import GHC.Exts (toList)

import Control.Monad.Trans (MonadIO)

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Oracle.Neural.Result (Result(Result))
import qualified Oracle.Neural.Result as Result

import Oracle.Neural.Query (Query(Query))
import qualified Oracle.Neural.Query as Query

import qualified Data.Vector as Vector

queryUniversalOracle :: (MonadIO m2) => ChoicePoint m a -> m2 Result
queryUniversalOracle cp = pure $ Result {
  Result.policy = fmap (\_->x) (Query.choices query),
  Result.value  = Just 0.5
  }
  where
    query = Query.fromChoicePoint cp
    x = 1.0 / (fromIntegral $ Vector.length $ Query.choices query)

trainUniversalOracle :: (MonadIO m2) => ChoicePoint m a -> Result -> m2 ()
trainUniversalOracle _ _ = pure ()
