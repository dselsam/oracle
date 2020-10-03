{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Oracle.Data.Graph where

import Data.Vector (Vector)

data Edge a = Edge {
  src :: Int,
  dst :: Int,
  val :: a
  } deriving (Eq, Ord, Show)

data Graph a b = Graph {
  nNodes :: Int,
  nodes  :: Vector b,
  edges  :: Vector (Edge a)
  } deriving (Eq, Ord, Show)

mapNodes :: (b -> c) -> Graph a b -> Graph a c
mapNodes f g@(Graph _ nodes _) = g { nodes = fmap f nodes }

mapEdges :: (a -> b) -> Graph a c -> Graph b c
mapEdges f g@(Graph _ _ edges) = g { edges = fmap (\(Edge i j x) -> Edge i j (f x)) edges }
