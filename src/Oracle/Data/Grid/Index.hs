module Oracle.Data.Grid.Index where

data Index = Index { row :: Int, col :: Int } deriving (Eq, Ord)

transpose :: Index -> Index
transpose (Index m n) = Index n m
