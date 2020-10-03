{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Replay search from list of choice indices.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Oracle.Search.Replay where

import Oracle.Data.Embeddable
import Oracle.Control.Monad.Search

import Oracle.Search.Result (Result(Result))
import qualified Oracle.Search.Result as Result

import Oracle.Search.Trace (Trace(Trace))
import qualified Oracle.Search.Trace as Trace

import Oracle.Search.Task (Task(Task))
import qualified Oracle.Search.Task as Task

import Oracle.Search.Decision (Decision(Decision))
import qualified Oracle.Search.Decision as Decision

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Oracle.Data.Deque (Deque)
import qualified Oracle.Data.Deque as Deque

import Debug.Trace (traceM)
import Data.Foldable (for_)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.State (StateT, evalStateT, get, gets, modify, State, execState)

replay :: (Monad m) => SearchT m a -> [Int] -> m [Decision]
replay f idxs = core f idxs [] where
  core :: (Monad m) => SearchT m a -> [Int] -> [Decision] -> m [Decision]
  core (SearchT f) idxs decisions = f >>= \case
    Done x -> pure decisions
    Choice (ChoicePoint cp cs) -> case idxs of
      [] -> error $ "[replay] no more indices: " ++ show cp
      (idx:idxs) -> do
        let k = snd $ cs Vector.! idx
        let decision = Decision cp (fmap fst cs) idx
        core k idxs (decision:decisions)
