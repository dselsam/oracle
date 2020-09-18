{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Oracle.BruteForce where

import Oracle.Data.Embeddable
import Oracle.SearchT

import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as Seq

import Oracle.Deque (Deque)
import qualified Oracle.Deque as Deque

import Data.Foldable (for_)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.State (StateT, evalStateT, get, gets, modify)

data Decision = Decision {
  snapshot  :: Embeddable,
  choices   :: Seq Embeddable,
  choiceIdx :: Int
  }

newtype Trace = Trace {
  decisions :: [Decision]
  }

data Task m a = Task {
  continuation :: SearchT m a,
  trace        :: Trace
  }

data Result a = Result {
  value :: a,
  trace :: Trace
  }

data SearchAlg = BreadthFirst | DepthFirst deriving (Eq, Ord, Show)

data BruteForceOptions = BruteForceOptions {
  maxResults   :: Int,
  maxSteps     :: Int,
  searchAlg    :: SearchAlg
  }

data BruteForceState m a = BruteForceState {
  tasks      :: Deque (Task m a),
  results    :: Seq (Result a),
  steps      :: Int
  }

type BruteForceT a m = ReaderT BruteForceOptions (StateT (BruteForceState m a) m)

bruteForceSearch :: (Monad m) => BruteForceOptions -> SearchT m a -> m (Seq (Result a))
bruteForceSearch opts psi = flip evalStateT s0 $ flip runReaderT opts $ search (maxSteps opts) where
  s0 = BruteForceState (Deque.singleton $ Task psi (Trace [])) Seq.empty 0

  search :: (Monad m) => Int -> BruteForceT a m (Seq (Result a))
  search 0    = gets results
  search fuel = (gets $ Deque.popFront . tasks) >>= \case
    Nothing         -> gets results
    Just (Task (SearchT k) trace, rest) -> do
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
            let task    = Task (snd $ Seq.index cs i) $ Trace $ (Decision cp (fmap fst cs) i) : decisions trace
            side <- asks searchAlg >>= \case
              BreadthFirst -> pure Deque.Back
              DepthFirst   -> pure Deque.Front
            modify $ \s -> s { tasks = Deque.push side task (tasks s) }
          search (fuel-1)
