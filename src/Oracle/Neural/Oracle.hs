{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

-}
module Oracle.Neural.Oracle where

-- TODO: move to new directory outside of Neural?
import Oracle.SearchT
import Oracle.Neural.Result (Result(Result))
import qualified Oracle.Neural.Result as Result
import Oracle.Neural.Query (Query(Query))
import qualified Oracle.Neural.Query as Query

import qualified Data.Vector as Vector

import Control.Monad.Trans (MonadIO)

data Oracle m m2 a = Oracle {
  query :: ChoicePoint m a -> m2 Result,
  train :: ChoicePoint m a -> Result -> m2 ()
  }

dummy :: (Monad m2) => Oracle m m2 a
dummy = Oracle {
  query = \cp -> do
      let query = Query.fromChoicePoint cp
      let x = 1.0 / (fromIntegral $ Vector.length $ Query.choices query)
      pure $ Result {
        Result.policy = fmap (\_->x) (Query.choices query),
        Result.value  = Just 0.0
        },
  train = \_ _ -> pure ()
  }
