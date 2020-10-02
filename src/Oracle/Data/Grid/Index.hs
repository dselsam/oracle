{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Oracle.Data.Grid.Index where

data Index = Index { row :: Int, col :: Int } deriving (Eq, Ord, Show)

instance Num Index where
  Index m1 n1 + Index m2 n2 = Index (m1+m2) (n1+n2)
  Index m1 n1 - Index m2 n2 = Index (m1-m2) (n1-n2)

scale :: Int -> Index -> Index
scale k (Index m n) = Index (k * m) (k * n)

transpose :: Index -> Index
transpose (Index m n) = Index n m
