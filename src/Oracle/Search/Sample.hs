{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Oracle.Search.Sample where

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

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Random (MonadRandom, getRandomR)

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Oracle.Data.Deque (Deque)
import qualified Oracle.Data.Deque as Deque

import Text.Printf (printf)
import Debug.Trace (traceM)
import Data.Foldable (for_)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.State (StateT, evalStateT, get, gets, modify, State, execState)

sample :: (MonadRandom m) => SearchT Identity a -> m (Maybe a)
sample (SearchT k) = case runIdentity k of
  Fail   -> pure Nothing
  Done x -> pure (Just x)
  Choice cp@(ChoicePoint snapshot cs) -> do
    i <- getRandomR (0, Vector.length cs - 1)
    sample (snd $ cs Vector.! i)
