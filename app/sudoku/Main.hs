{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Console.CmdArgs

import Oracle.Control.Monad.Search

import Oracle.Search.BestFirst (BestFirstOptions(..), bestFirstSearch)
import qualified Oracle.Search.Result as Result
import qualified Oracle.Search.Trace as Trace

import Oracle.Examples.Puzzles.Sudoku.Board (Board)
import qualified Oracle.Examples.Puzzles.Sudoku.Board as Board
import qualified Oracle.Examples.Puzzles.Sudoku.Solve as Solve
import qualified Oracle.Examples.Puzzles.Sudoku.Data as Data
import Oracle.Examples.Puzzles.Sudoku.Data (BoardPair(BoardPair), SearchPair(SearchPair))

import qualified Oracle.Neural.Universal as Universal

import qualified Data.Vector as Vector
import qualified Data.Sequence as Seq

import Lens.Micro ((^.))
import Text.Printf (printf)
import Control.Monad (when)
import Control.Monad.State (evalStateT)
import Data.Foldable (for_)
import Data.List.Split (splitOn)

import qualified Proto.Response as P
import qualified Proto.Response_Fields as Response
import qualified Proto.Response_Fields as Prediction

data Args = Args {
  host           :: String,
  port           :: String,
  sudokuCSV      :: FilePath,
  nEpochs        :: Int,
  nTrain         :: Int,
  nValid         :: Int
  } deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs = Args {
  host             = "localhost" &= name "host",
  port             = "10000"     &= name "port",
  -- expecting a file in the format of https://www.kaggle.com/bryanpark/sudoku
  sudokuCSV        = "thirdparty/data/sudoku.csv" &= name "sudokuCSV",
  nEpochs          = (1 :: Int)    &= name "nEpochs",
  nTrain           = (1000 :: Int) &= name "nTrain",
  nValid           = (10 :: Int)   &= name "nValid"
  }


genData :: Args -> IO ()
genData args = do
  putStrLn "initializing universal oracle..."
  execute $ Universal.init

  for_ [(1::Int)..(nEpochs args)] $ \epoch -> do
    putStrLn $ printf "Beginning epoch: %d" epoch
    lines :: [String] <- fmap (tail . lines) $ readFile (sudokuCSV args)
    let boards :: [(Board, Board)] = flip map lines $ (\[start, end] -> (Board.fromStringOfInts start, Board.fromStringOfInts end)) . splitOn ","

    let train = take (nTrain args) (drop (nValid args) boards)

    when (epoch == 1) $ do
      validate "train" $ take (nValid args) train
      validate "valid" $ take (nValid args) boards

    for_ (zip [(1::Int)..] train) $ \(i, (start, end)) -> do
      for_ (zip [(1::Int)..] Data.searchPairs) $ \(j, searchPair) -> do
        decisions <- Data.genData (BoardPair start end) searchPair
        putStrLn $ printf "train %d:%d %d" i j (length decisions)
        execute $ Universal.train 1 decisions

    validate "train" $ take (nValid args) train
    validate "valid" $ take (nValid args) boards

  where
    execute = Universal.execute (host args) (port args)

    validate :: String -> [(Board, Board)] -> IO ()
    validate msg valid = do
      for_ (zip [(1::Int)..] valid) $ \(i, (start, _)) -> do
        for_ (zip [(1::Int)..] Data.searchPairs) $ \(j, SearchPair fsearch _) -> do
          nSteps <- bestFirst fsearch start
          putStrLn $ printf "valid[%s] %d:%d %d" msg i j nSteps

    -- TODO(dselsam): weave in the max number of steps
    bestFirst fsearch start = do
      results <- bestFirstSearch (BestFirstOptions 1 1000) predict (evalStateT (Solve.solve fsearch) start)
      if Seq.null results then pure (-1) else
        pure . length . Trace.decisions . Result.trace . flip Seq.index 0 $ results

    predict choicePoint = do
      response <- execute $ Universal.predict [choicePoint]
      -- TODO(dselsam): there must be a way to read directly as a vector!
      pure . Vector.fromList $ (head (response ^. Response.predictions) ^. Prediction.policy)

main :: IO ()
main = do
  putStrLn "-- Sudoku --"
  args <- cmdArgs defaultArgs
  genData args
