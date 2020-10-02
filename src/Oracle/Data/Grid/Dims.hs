{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Oracle.Data.Grid.Dims where

import Oracle.Data.Grid.Index (Index(Index))
import qualified Oracle.Data.Grid.Index as Index

data Dims = Dims { nRows :: Int, nCols :: Int } deriving (Eq, Ord, Show)

inBounds :: Dims -> Index -> Bool
inBounds (Dims m n) (Index r c) = r >= 0 && r < m && c >= 0 && c < n

nCells :: Dims -> Int
nCells (Dims m n) = m * n

transpose :: Dims -> Dims
transpose (Dims m n) = Dims n m

int2index :: Dims -> Int -> Index
int2index (Dims m n) i
  | n > 0 && div i n < m = Index (div i n) (mod i n)
  | otherwise = error "int2index div by 0"

index2int :: Dims -> Index -> Int
index2int ds@(Dims m n) idx@(Index row col)
  | inBounds ds idx = row * n + col
  | otherwise = error "index2int idx not in bounds"
