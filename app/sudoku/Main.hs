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
import Control.Concurrent (threadDelay)

import Oracle.Control.Monad.Search

import Oracle.Search.BestFirst (BestFirstOptions(..), bestFirstSearch)
import qualified Oracle.Search.Interactive as Search
import qualified Oracle.Search.Result as Result
import qualified Oracle.Search.Trace as Trace

import Oracle.Examples.Puzzles.Sudoku.Board (Board)
import qualified Oracle.Examples.Puzzles.Sudoku.Board as Board
import qualified Oracle.Examples.Puzzles.Sudoku.Solve as Solve
import qualified Oracle.Examples.Puzzles.Sudoku.Data as Data
import Oracle.Examples.Puzzles.Sudoku.Data (BoardPair(BoardPair), SearchPair(SearchPair))

import qualified Oracle.Neural.Universal as Universal

import Data.Maybe (isJust, fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Sequence as Seq

import Data.IORef (newIORef, modifyIORef', readIORef)
import Lens.Micro ((^.))
import Text.Printf (printf)
import Control.Monad (when, unless)
import Control.Monad.State (evalStateT, execStateT)
import Data.Foldable (for_)
import Data.List.Split (splitOn)

import qualified Data.List as List

import System.Console.Regions (displayConsoleRegions)
import qualified System.Console.AsciiProgress as Progress

import qualified Proto.Protos.Command as P
import qualified Proto.Protos.Response as P
import qualified Proto.Protos.Response_Fields as Response
import qualified Proto.Protos.Response_Fields as Prediction

data Args = Args {
  interactive    :: Bool,
  problemIdx     :: Int,
  selectIdx      :: Int,

  host           :: String,
  port           :: String,
  loadFilename   :: Maybe String,
  saveFilename   :: Maybe String,

  sudokuCSV      :: FilePath,
  nEpochs        :: Int,
  nTrain         :: Int,
  nHeldout       :: Int,
  nSearchTrain   :: Int,
  nSearchHeldout :: Int,
  maxSearchSteps :: Int
  } deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs = Args {
  interactive      = False &= name "interactive",
  problemIdx       = 0 &= name "problemIdx",
  selectIdx        = 0 &= name "selectIdx",

  host             = "localhost" &= name "host",
  port             = "10000"     &= name "port",

  loadFilename     = Nothing &= name "loadFilename",
  saveFilename     = Nothing &= name "saveFilename",

  -- expecting a file in the format of https://www.kaggle.com/bryanpark/sudoku
  sudokuCSV        = "thirdparty/data/sudoku.csv" &= name "sudokuCSV",
  nEpochs          = 10    &= name "nEpochs",
  nTrain           = 1000  &= name "nTrain",
  nHeldout         = 10    &= name "nHeldout",
  nSearchTrain     = 5     &= name "nSearchTrain",
  nSearchHeldout   = 5     &= name "nSearchValid",
  maxSearchSteps   = 1000  &= name "maxSearchSteps"
  }


genData :: Args -> IO ()
genData args = do
  maybeLoad args

  for_ [(1::Int)..(nEpochs args)] $ \epoch -> do
    (trains, heldouts) <- fmap (splitTrainHeldout args) $ readBoards args

    runSearch epoch True  $ take (nSearchTrain args) trains
    runSearch epoch False $ take (nSearchHeldout args) heldouts
    runEpoch epoch True  trains
    runEpoch epoch False heldouts

  maybeSave args

  where
    runSearch :: Int -> Bool -> [BoardPair] -> IO ()
    runSearch epoch train boards = unless (null boards) $ displayConsoleRegions $ do
      let nResults = length boards * length Data.searchPairs
      pg <- mkProgressBar nResults

      nSolved_ <- newIORef (0 :: Int)
      nSteps_  <- newIORef (0 :: Int)

      for_ boards $ \(BoardPair start _) -> do
        for_ Data.searchPairs $ \(SearchPair fsearch _) -> do
          result <- bestFirst fsearch start
          case result of
            Nothing     -> pure ()
            Just nSteps -> do
              modifyIORef' nSolved_ (+1)
              modifyIORef' nSteps_ $ (+nSteps)
          Progress.tick pg

      Progress.complete pg
      threadDelay 100 -- awkward, just so the progress bar completes

      nSteps  <- readIORef nSteps_
      nSolved <- readIORef nSolved_
      let avgNumSteps :: Float = (fromIntegral nSteps) / (fromIntegral nSolved)
      putStrLn $ printf "[eval:%d:%s] %4d %6.2f" epoch (if train then "1" else "0") nSolved avgNumSteps

      where
        bestFirst fsearch start = do
          results <- bestFirstSearch (BestFirstOptions 1 (maxSearchSteps args)) (oracle args) (evalStateT (Solve.solve fsearch) start)
          if Seq.null results then pure Nothing else
            pure . Just . length . Trace.decisions . Result.trace . flip Seq.index 0 $ results

    runEpoch :: Int -> Bool -> [BoardPair] -> IO ()
    runEpoch epoch train boards = unless (null boards) $ displayConsoleRegions $ do
      let nQueries = length boards * length Data.searchPairs
      pg    <- mkProgressBar nQueries
      loss_ <- newIORef 0.0

      for_ boards $ \boardPair -> do
        for_ Data.searchPairs $ \searchPair -> do
          decisions <- Data.genData boardPair searchPair
          let cmd = (if train then Universal.train 1 else Universal.valid) decisions
          response <- execute args cmd
          modifyIORef' loss_ $ \loss -> loss + (response ^. Response.loss / (fromIntegral nQueries))
          Progress.tick pg

      Progress.complete pg
      threadDelay 100 -- awkward, just so the progress bar completes

      loss <- readIORef loss_
      putStrLn $ printf "[loss:%d:%s] %6.4f" epoch (if train then "1" else "0") loss

    mkProgressBar k = Progress.newProgressBar Progress.def {
      Progress.pgTotal        = fromIntegral k
      }

solveInteractive :: Args -> IO ()
solveInteractive args = do
  maybeLoad args
  BoardPair start end <- fmap (\xs -> xs List.!! (problemIdx args)) $ readBoards args
  putStrLn $ "Start: " ++ show start
  putStrLn $ "End:   " ++ show end
  let SearchPair fselect _ = Data.searchPairs List.!! (selectIdx args)
  Search.interactive [("universal", oracle args)] (execStateT (Solve.solve fselect) start)
  pure ()

splitTrainHeldout :: Args -> [BoardPair] -> ([BoardPair], [BoardPair])
splitTrainHeldout args boards = (take (nTrain args) boards, take (nHeldout args) (drop (nTrain args) boards))

--oracle :: Args -> ChoicePoint -> IO (Vector Float)
oracle args choicePoint = do
  response <- execute args $ Universal.predict [choicePoint]
  -- TODO(dselsam): there must be a way to read directly as a vector!
  pure . Vector.fromList $ (head (response ^. Response.predictions) ^. Prediction.policy)

readBoards :: Args -> IO [BoardPair]
readBoards args = do
  lines :: [String] <- fmap (tail . lines) $ readFile (sudokuCSV args)
  let boards' :: [(Board, Board)] = flip map lines $ (\[start, end] -> (Board.fromStringOfInts start, Board.fromStringOfInts end)) . splitOn ","
  pure $ map (uncurry BoardPair) boards'

maybeLoad :: Args -> IO ()
maybeLoad args = case loadFilename args of
  Just filename -> execute args (Universal.load filename) *> pure ()
  Nothing -> pure ()

maybeSave :: Args -> IO ()
maybeSave args = case saveFilename args of
  Just filename -> execute args (Universal.save filename) *> pure ()
  Nothing -> pure ()

execute :: Args -> P.Command -> IO P.Response
execute args = Universal.execute (host args) (port args)

main :: IO ()
main = do
  putStrLn "-- Sudoku --"
  args <- cmdArgs defaultArgs
  case () of
    _
      | interactive args -> solveInteractive args
      | otherwise        -> genData args
