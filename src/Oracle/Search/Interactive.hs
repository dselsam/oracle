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
module Oracle.Search.Interactive where

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

import Text.Printf (printf)
import Debug.Trace (traceM)
import Data.Foldable (for_)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.State (StateT, evalStateT, get, gets, modify, State, execState)

type Oracle m a = ChoicePoint m a -> m (Vector Float)

interactive :: (Monad m, MonadIO m, Show a) => [(String, Oracle m a)] -> SearchT m a -> m (Maybe a)
interactive oracles (SearchT k) = k >>= \case
  Fail   -> do
    liftIO . putStrLn $ "\nFail\n"
    pure Nothing

  Done x -> do
    liftIO . putStrLn $ "\nDone:\n" ++ show x
    pure (Just x)

  Choice cp@(ChoicePoint snapshot cs) -> do
    liftIO . putStrLn $ "\nSnapshot:\n" ++ show snapshot ++ "\n"
    liftIO . putStrLn $ "\nChoices:\n"

    pis <- mapM (\(_, oracle) -> oracle cp) oracles

    for_ (zip [0..] (Vector.toList cs)) $ \(i, (c, _)) -> do
      liftIO . putStr $ printf "  [%3d] %s { " i (show c)
      for_ (zip oracles pis) $ \((name, _), pi) -> do
        liftIO . putStr $ printf "%s:%5.3f " name (pi Vector.! i)
      liftIO . putStr $ "}\n"

    liftIO . putStrLn $ printf "\nPlease enter choice (%d:%d):\n" (0 :: Int) (length cs)
    user <- liftIO getLine
    let i = (read user :: Int)
    interactive oracles (snd $ cs Vector.! i)
