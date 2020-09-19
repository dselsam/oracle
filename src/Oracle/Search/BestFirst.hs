{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Oracle.Search.BestFirst where

import Oracle.Data.Embeddable
import Oracle.SearchT

import Oracle.Search.Result (Result(Result))
import qualified Oracle.Search.Result as Result

import Oracle.Search.Trace (Trace(Trace))
import qualified Oracle.Search.Trace as Trace

import Oracle.Search.Task (Task(Task))
import qualified Oracle.Search.Task as Task

import Oracle.Search.Decision (Decision(Decision))
import qualified Oracle.Search.Decision as Decision

import qualified Oracle.Neural as Neural

import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as Seq

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Foldable (for_)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.State (StateT, evalStateT, get, gets, modify)

import Data.PQueue.Prio.Max (MaxPQueue)
import qualified Data.PQueue.Prio.Max as PQ

type PQ = MaxPQueue

data BestFirstOptions = BestFirstOptions {
  maxResults   :: Int,
  maxSteps     :: Int
  }

data BestFirstState m a = BestFirstState {
  tasks      :: PQ Float (Task m a),
  results    :: Seq (Result a)
  }

type BestFirstT a m = ReaderT BestFirstOptions (StateT (BestFirstState m a) m)

type Oracle m a = ChoicePoint m a -> m (Vector Float)

bestFirstSearch :: (Monad m) => BestFirstOptions -> Oracle m a -> SearchT m a -> m (Seq (Result a))
bestFirstSearch opts oracle psi = flip evalStateT s0 $ flip runReaderT opts $ search (maxSteps opts) where
  s0 = BestFirstState (PQ.singleton 0.0 $ Task psi (Trace [])) Seq.empty

  search 0    = gets results
  search fuel = (gets $ PQ.maxViewWithKey . tasks) >>= \case
    Nothing -> gets results

    Just ((cumLogProb, Task (SearchT k) trace), rest) -> do
      modify $ \s -> s { tasks = rest }
      (lift . lift $ k) >>= \case
        Fail -> search (fuel-1)
        Done x -> do
          modify $ \s -> s { results = results s |> Result x trace }
          maxResults <- asks maxResults
          finished <- gets $ (== maxResults) . Seq.length . results
          if finished then gets results else search (fuel-1)
        Choice cp@(ChoicePoint snap cs) -> do
          -- TODO: for now we ignore the valueHead
          pi <- lift . lift $ oracle cp
          for_ [1..Seq.length cs] $ \i' -> do
            let i = Seq.length cs - i'
            let p = pi Vector.! i
            let newCumLogProb = cumLogProb + log p
            let task = Task (snd $ Seq.index cs i) $ Trace $ (Decision snap (fmap fst cs) i p) : Trace.decisions trace
            modify $ \s -> s { tasks = PQ.insert newCumLogProb task (tasks s) }
          search (fuel-1)
