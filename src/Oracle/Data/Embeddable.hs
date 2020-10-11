{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

We want to compose SearchT programs that use different datastructures for their snapshots and choices,
while requiring that they are all embeddable, one way or another.
The natural solution may seem to return a bundle (α, HasEmbed α), but this bumps universe levels and causes headaches.
For now, we commit to an abstract representation of things that can be embedded, that can live in Type.
-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
module Oracle.Data.Embeddable where

import Oracle.Data.Grid (Grid)
import qualified Oracle.Data.Grid as Grid

import Data.Sequence (Seq)

import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as Set

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Vector as Vector

import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Proto.Embeddable as P

import GHC.Exts (toList)
import qualified Data.Text as Text
import Oracle.Data.Grid (Grid(Grid), Index(Index), Dims(Dims))
import Oracle.Data.Graph (Graph(Graph), Edge(Edge))
import qualified Oracle.Data.Graph as Graph

data Embeddable =
  EUnit ()
  | EBool Bool
  | EChar Char
  | EInt Int
  | EString String
  | EPair (Embeddable, Embeddable)
  | EMaybe (Maybe Embeddable)
  | EList [Embeddable]
  | ESeq (Seq Embeddable)
  | ESet [Embeddable]
  | EMap [(Embeddable, Embeddable)]
  | EGrid (Grid Embeddable)
  | EGraph (Graph Embeddable Embeddable)
  | ERecord String [(String, Embeddable)]
  | EData String [(String, Embeddable)] -- TODO: more data?
  deriving (Eq, Ord, Show)

class HasToEmbeddable a where
  toEmbeddable :: a -> Embeddable

instance HasToEmbeddable () where
  toEmbeddable   = EUnit

instance HasToEmbeddable Bool where
  toEmbeddable = EBool

instance HasToEmbeddable Char where
  toEmbeddable = EChar

instance HasToEmbeddable Int where
  toEmbeddable = EInt

-- instance {-# OVERLAPS #-} HasToEmbeddable String where
--   toEmbeddable = EString

instance (HasToEmbeddable a, HasToEmbeddable b) => HasToEmbeddable (a, b) where
  toEmbeddable (a, b) = EPair (toEmbeddable a, toEmbeddable b)

instance (HasToEmbeddable a) => HasToEmbeddable (Maybe a) where
  toEmbeddable m = EMaybe (fmap toEmbeddable m)

instance {-# OVERLAPPABLE #-} (HasToEmbeddable a) => HasToEmbeddable [a] where
  toEmbeddable xs = EList (map toEmbeddable xs)

instance (HasToEmbeddable a) => HasToEmbeddable (Seq a) where
  toEmbeddable xs = ESeq (fmap toEmbeddable xs)

instance (HasToEmbeddable a) => HasToEmbeddable (Set a) where
  toEmbeddable xs = ESet (map toEmbeddable $ Set.toList xs)

instance (HasToEmbeddable a) => HasToEmbeddable (HashSet a) where
  toEmbeddable xs = ESet (map toEmbeddable $ HashSet.toList xs)

instance (HasToEmbeddable a, HasToEmbeddable b) => HasToEmbeddable (Map a b) where
  toEmbeddable = EMap . map (\(k, v) -> (toEmbeddable k, toEmbeddable v)) . Map.assocs

instance (HasToEmbeddable a) => HasToEmbeddable (Grid a) where
  toEmbeddable xs = EGrid (fmap toEmbeddable xs)

instance HasToEmbeddable Index where
  toEmbeddable (Index row col) = ERecord "Index" [
    ("row", toEmbeddable row),
    ("col", toEmbeddable col)
    ]

instance (HasToEmbeddable a, HasToEmbeddable b) => HasToEmbeddable (Graph a b) where
  toEmbeddable = EGraph . Graph.mapEdges toEmbeddable . Graph.mapNodes toEmbeddable

data Attrs = Attrs String [(String, Embeddable)]

instance HasToEmbeddable Attrs where
  toEmbeddable (Attrs n cs) = ERecord n cs

toProto :: Embeddable -> P.Embeddable
toProto x = case x of
  EBool b           -> defMessage & #b .~ b
  EChar c           -> defMessage & #char .~ (fromIntegral $ Char.ord c)
  EInt n            -> defMessage & #n .~ fromIntegral n
  EString s         -> defMessage & #s .~ Text.pack s
  EMaybe m          -> defMessage & #maybe .~ maybe2proto m
  EPair p           -> defMessage & #pair .~ pair2proto p
  EList xs          -> defMessage & #list .~ (defMessage & #elems .~ map toProto xs)
  ESeq xs           -> defMessage & #array .~ (defMessage & #elems .~ (toList $ fmap toProto xs))
  ESet s            -> defMessage & #set .~ (defMessage & #elems .~ (map toProto s))
  EMap assocs       -> defMessage & #map .~ (defMessage & #assocs .~ (map pair2proto assocs))
  EGrid g           -> defMessage & #grid .~ grid2proto g
  EGraph g          -> defMessage & #graph .~ graph2proto g
  ERecord n fields  -> defMessage & #record .~ (defMessage & #name .~ Text.pack n & #fields .~ (flip map fields $ \(k, v) -> defMessage & #name .~ Text.pack k & #value .~ toProto v))

  where
    pair2proto (x, y) = defMessage
      & #fst .~ toProto x
      & #snd .~ toProto y

    maybe2proto m = case m of
      Nothing -> defMessage
      Just x  -> defMessage & #val .~ toProto x

    grid2proto (Grid (Dims nRows nCols) elems) = defMessage
      & #nRows .~ fromIntegral nRows
      & #nCols .~ fromIntegral nCols
      & #elems .~ toList (fmap toProto elems)

    graph2proto (Graph nNodes nodes edges) = defMessage
      & #nNodes .~ fromIntegral nNodes
      & #nodes  .~ (Vector.toList $ fmap toProto nodes)
      & #edges  .~ (Vector.toList $ fmap edge2proto edges)

    edge2proto (Edge src dst val) = defMessage
      & #src    .~ fromIntegral src
      & #dst    .~ fromIntegral dst
      & #val    .~ toProto val
