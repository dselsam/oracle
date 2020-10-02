{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Oracle.Data.Grid.Index where

data Index = Index { row :: Int, col :: Int } deriving (Eq, Ord)

transpose :: Index -> Index
transpose (Index m n) = Index n m
