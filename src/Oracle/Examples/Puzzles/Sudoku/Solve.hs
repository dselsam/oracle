{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Oracle.Examples.Puzzles.Sudoku.Solve where

import Oracle.Data.Embeddable
import Oracle.Control.Monad.Search

import Oracle.Search.Decision (Decision(Decision))

import Oracle.Data.Grid.Index (Index(Index))
import qualified Oracle.Data.Grid.Index as Index

import Oracle.Examples.Puzzles.Sudoku.Board (Board(Board), Value(Value), SubgridIndex(SubgridIndex))
import qualified Oracle.Examples.Puzzles.Sudoku.Board as Board
import Control.Monad.State (StateT, runStateT, get, gets, put, modify)

import qualified Data.Set as Set

type SolveM = StateT Board (SearchT IO)

solve :: SolveM () -> SolveM ()
solve f = Board.isFilled >>= \case
  True   -> pure ()
  False  -> f *> solve f

selectRCV :: SolveM ()
selectRCV = do
  i <- select "row" [0..8]
  j <- select "col" [0..8]
  x <- select "val" $ map Value [1..9]
  Board.set (Index i j) x

selectVRC :: SolveM ()
selectVRC = do
  x <- select "val" $ map Value [1..9]
  i <- select "row" [0..8]
  j <- select "col" [0..8]
  Board.set (Index i j) x

selectOIV :: SolveM ()
selectOIV = do
  oi <- select "outer row" [0..2]
  oj <- select "outer col" [0..2]
  ii <- select "inner row" [0..2]
  ij <- select "inner col" [0..2]
  x  <- select "val"       $ map Value [1..9]
  Board.set (Board.subgrid2idx (SubgridIndex (Index oi oj) (Index ii ij))) x

selectEmpty :: SolveM ()
selectEmpty = do
  emptys <- gets Board.emptys
  idx   <- select "index" (Set.toList emptys)
  x     <- select "val" $ map Value [1..9]
  Board.set idx x

select :: (HasToEmbeddable a) => String -> [a] -> SolveM a
select n xs = do
  board <- get
  oneOf (Attrs n [("board", toEmbeddable board)]) $ map (\x -> (x, x)) xs
