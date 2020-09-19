{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Oracle.Data.Embeddable where

import Oracle.Data.Grid (Grid)
import qualified Oracle.Data.Grid as Grid

import Data.Sequence (Seq)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

{-
We want to compose SearchT programs that use different datastructures for their snapshots and choices,
while requiring that they are all embeddable, one way or another.
The natural solution may seem to return a bundle (α, HasEmbed α), but this bumps universe levels and causes headaches.
For now, we commit to an abstract representation of things that can be embedded, that can live in Type.
-}

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

instance HasToEmbeddable String where
  toEmbeddable = EString

instance (HasToEmbeddable a, HasToEmbeddable b) => HasToEmbeddable (a, b) where
  toEmbeddable (a, b) = EPair (toEmbeddable a, toEmbeddable b)

instance (HasToEmbeddable a) => HasToEmbeddable [a] where
  toEmbeddable xs = EList (map toEmbeddable xs)

instance (HasToEmbeddable a) => HasToEmbeddable (Seq a) where
  toEmbeddable xs = ESeq (fmap toEmbeddable xs)

instance (HasToEmbeddable a) => HasToEmbeddable (Set a) where
  toEmbeddable xs = ESet (Set.map toEmbeddable xs)

instance (HasToEmbeddable a, HasToEmbeddable b) => HasToEmbeddable (Map a b) where
  toEmbeddable = EMap . map (\(k, v) -> (toEmbeddable k, toEmbeddable v)) . Map.assocs

instance (HasToEmbeddable a) => HasToEmbeddable (Grid a) where
  toEmbeddable xs = EGrid (Grid.map (\_ -> toEmbeddable) xs)
