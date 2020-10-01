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

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Vector as Vector

import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Proto.Protos.Embeddable as P
import qualified Proto.Protos.ChoicePoint as P
import qualified Proto.Protos.Result as P
import qualified Proto.Protos.DataPoint as P

import GHC.Exts (toList)
import qualified Data.Text as Text
import Oracle.Data.Grid.Grid (Grid(Grid))
import Oracle.Data.Grid.Dims (Dims(Dims))

data Embeddable =
  EUnit ()
  | EBool Bool
  | EInt Int
  | EString String
  | EPair (Embeddable, Embeddable)
  | EList [Embeddable]
  | ESeq (Seq Embeddable)
  | ESet (Set Embeddable)
  | EMap [(Embeddable, Embeddable)]
  | EGrid (Grid Embeddable)
  | ERecord String [(String, Embeddable)]
  | EData String [(String, Embeddable)] -- TODO: more data?
  deriving (Eq, Ord, Show)

class HasToEmbeddable a where
  toEmbeddable :: a -> Embeddable

instance HasToEmbeddable () where
  toEmbeddable   = EUnit

instance HasToEmbeddable Bool where
  toEmbeddable = EBool

instance HasToEmbeddable Int where
  toEmbeddable = EInt

instance {-# OVERLAPS #-} HasToEmbeddable String where
  toEmbeddable = EString

instance (HasToEmbeddable a, HasToEmbeddable b) => HasToEmbeddable (a, b) where
  toEmbeddable (a, b) = EPair (toEmbeddable a, toEmbeddable b)

instance {-# OVERLAPPABLE #-} (HasToEmbeddable a) => HasToEmbeddable [a] where
  toEmbeddable xs = EList (map toEmbeddable xs)

instance (HasToEmbeddable a) => HasToEmbeddable (Seq a) where
  toEmbeddable xs = ESeq (fmap toEmbeddable xs)

instance (HasToEmbeddable a) => HasToEmbeddable (Set a) where
  toEmbeddable xs = ESet (Set.map toEmbeddable xs)

instance (HasToEmbeddable a, HasToEmbeddable b) => HasToEmbeddable (Map a b) where
  toEmbeddable = EMap . map (\(k, v) -> (toEmbeddable k, toEmbeddable v)) . Map.assocs

instance (HasToEmbeddable a) => HasToEmbeddable (Grid a) where
  toEmbeddable xs = EGrid (Grid.map (\_ -> toEmbeddable) xs)

data Attrs = Attrs String [(String, Embeddable)]

instance HasToEmbeddable Attrs where
  toEmbeddable (Attrs n cs) = ERecord n cs

toProto :: Embeddable -> P.Embeddable
toProto x = case x of
  EBool b           -> defMessage & #b .~ b
  EInt n            -> defMessage & #n .~ fromIntegral n
  EString s         -> defMessage & #s .~ Text.pack s
  EPair p           -> defMessage & #pair .~ pair2proto p
  EList xs          -> defMessage & #list .~ (defMessage & #elems .~ map toProto xs)
  ESeq xs           -> defMessage & #array .~ (defMessage & #elems .~ (toList $ fmap toProto xs))
  ESet s            -> defMessage & #set .~ (defMessage & #elems .~ (map toProto $ Set.toList s))
  EMap assocs       -> defMessage & #map .~ (defMessage & #assocs .~ (map pair2proto assocs))
  EGrid g           -> defMessage & #grid .~ grid2proto g
  ERecord n fields  -> defMessage & #record .~ (defMessage & #name .~ Text.pack n & #fields .~ (flip map fields $ \(k, v) -> defMessage & #name .~ Text.pack k & #value .~ toProto v))

  where
    pair2proto (x, y) = defMessage & #fst .~ toProto x & #snd .~ toProto y
    grid2proto (Grid (Dims nRows nCols) elems) = defMessage & #nRows .~ fromIntegral nRows & #nCols .~ fromIntegral nCols & #elems .~ Vector.toList (fmap toProto elems)
