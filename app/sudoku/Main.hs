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

import qualified Oracle.Examples.Puzzles.Sudoku.Board as Board
import qualified Oracle.Examples.Puzzles.Sudoku.Solve as Solve
import qualified Oracle.Examples.Puzzles.Sudoku.Data as Data
import Oracle.Examples.Puzzles.Sudoku.Data (BoardPair(BoardPair), SearchPair(SearchPair))

import Text.Printf (printf)
import Control.Monad (replicateM_)
import Data.Foldable (for_)
import Data.List.Split (splitOn)

data Args = Args {
  argsGenData          :: Bool,
  argsTrainOracle      :: Bool,
  argsSolveInteractive :: Bool,

  argsSudokuCSV        :: FilePath,
  argsNumGenIters      :: Int,
  argsNumProblems      :: Int
  } deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs = Args {
  argsGenData          = False &= name "genData",
  argsTrainOracle      = False &= name "trainOracle",
  argsSolveInteractive = False &= name "solveInteractive",

  -- expecting a file in the format of https://www.kaggle.com/bryanpark/sudoku
  argsSudokuCSV        = "thirdparty/data/sudoku.csv" &= name "sudokuCSV",
  argsNumGenIters      = (1 :: Int)    &= name "nGenIters",
  argsNumProblems      = (1000 :: Int) &= name "nProblems"
  }


genData :: FilePath -> Int -> Int -> IO ()
genData sudokuCSV nIters nProblems = replicateM_ nIters $ do
  csv <- readFile sudokuCSV
  for_ (zip [(1::Int)..] (take nProblems $ tail $ lines csv)) $ \(i, line) -> do
    putStrLn $ printf "%d" i
    let [start', end'] = splitOn "," line
    let start = Board.fromStringOfInts start'
    let end   = Board.fromStringOfInts end'
    for_ Data.searchPairs $ \searchPair -> do
      decisions <- Data.genData (BoardPair start end) searchPair
      -- TODO: train
      pure ()

trainOracle :: IO ()
trainOracle = error "NYI"

solveInteractive :: IO ()
solveInteractive = error "NYI"

main :: IO ()
main = do
  putStrLn "-- Sudoku --"
  args <- cmdArgs defaultArgs
  case () of
    _ | argsGenData args          -> genData (argsSudokuCSV args) (argsNumGenIters args) (argsNumProblems args)
    _ | argsTrainOracle args      -> trainOracle
    _ | argsSolveInteractive args -> solveInteractive
