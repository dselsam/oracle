{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Oracle.Search.BruteForce where

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

import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as Seq

import Oracle.Data.Deque (Deque)
import qualified Oracle.Data.Deque as Deque

import Debug.Trace (traceM)
import Data.Foldable (for_)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.State (StateT, evalStateT, get, gets, modify)

data SearchAlg = BreadthFirst | DepthFirst deriving (Eq, Ord, Show)

data BruteForceOptions = BruteForceOptions {
  maxResults   :: Int,
  maxSteps     :: Int,
  searchAlg    :: SearchAlg
  }

data BruteForceState m a = BruteForceState {
  tasks      :: Deque (Task m a),
  results    :: Seq (Result a)
  }

type BruteForceT a m = ReaderT BruteForceOptions (StateT (BruteForceState m a) m)

bruteForceSearch :: (Monad m) => BruteForceOptions -> SearchT m a -> m (Seq (Result a))
bruteForceSearch opts psi = flip evalStateT s0 $ flip runReaderT opts $ search (maxSteps opts) where
  s0 = BruteForceState (Deque.singleton $ Task psi (Trace [])) Seq.empty

  search :: (Monad m) => Int -> BruteForceT a m (Seq (Result a))
  search 0    = gets results
  search fuel = (gets $ Deque.popFront . tasks) >>= \case
    Nothing -> gets results

    Just (Task (SearchT k) trace, rest) -> do
      -- traceM $ "[search] " ++ (show $ reverse $ map (\(Decision _ cs i _) -> Seq.index cs i) $ Trace.decisions trace)
      modify $ \s -> s { tasks = rest }
      (lift . lift $ k) >>= \case
        Fail -> search (fuel-1)
        Done x -> do
          modify $ \s -> s { results = results s |> Result x trace }
          maxResults <- asks maxResults
          finished <- gets $ (== maxResults) . Seq.length . results
          if finished then gets results else search (fuel-1)
        Choice (ChoicePoint cp cs) -> do
          for_ [1..Seq.length cs] $ \i' -> do
            i <- asks searchAlg >>= \case DepthFirst -> pure $ Seq.length cs - i'; BreadthFirst -> pure $ i' - 1
            let task = Task (snd $ Seq.index cs i) $ Trace $ (Decision cp (fmap fst cs) i) 0.0 : Trace.decisions trace
            side <- asks searchAlg >>= \case
              BreadthFirst -> pure Deque.Back
              DepthFirst   -> pure Deque.Front
            modify $ \s -> s { tasks = Deque.push side task (tasks s) }
          search (fuel-1)
