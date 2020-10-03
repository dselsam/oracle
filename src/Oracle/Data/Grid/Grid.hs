{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Oracle.Data.Grid.Grid where

import Control.Monad (guard)
import Control.Monad.Identity (Identity, runIdentity)

import Oracle.Data.Grid.Index (Index(..))
import qualified Oracle.Data.Grid.Index as Index

import Oracle.Data.Grid.Dims (Dims(..))
import qualified Oracle.Data.Grid.Dims as Dims

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import qualified Data.List as List

data Grid a = Grid {
  dims    :: Dims,
  elems   :: Seq a
  } deriving (Eq, Ord)


instance Show a => Show (Grid a) where
  show grid = "\n" ++ List.intercalate "\n" rows where
    rows = map mkCol [0..8]
    mkCol i = List.intercalate "|" (map (\j -> show $ get (Index i j) grid) [0..8])

fromSeq :: Dims -> Seq a -> Grid a
fromSeq dims v | Dims.nCells dims == Seq.length v = Grid dims v

fromList :: Dims -> [a] -> Grid a
fromList dims elems = fromSeq dims (Seq.fromList elems)

empty :: Dims -> a -> Grid a
empty dims x = Grid {
  dims  = dims,
  elems = Seq.replicate (Dims.nCells dims) x
  }

toOffset :: Grid a -> Index -> Int
toOffset g (Index i j) = i * Dims.nCols (dims g) + j

toOffsetSafe :: Grid a -> Index -> Maybe Int
toOffsetSafe g idx = do
  guard $ Dims.inBounds (dims g) idx
  pure $ toOffset g idx

get :: Index -> Grid a -> a
get idx g = Seq.index (elems g) (toOffset g idx)

set :: Index -> a -> Grid a -> Grid a
set idx x g = g { elems = Seq.update (toOffset g idx) x (elems g) }

instance Functor Grid where
  fmap f (Grid dims elems) = Grid dims (fmap f elems)

instance Foldable Grid where
  foldr f init (Grid dims elems) = foldr f init elems

instance Traversable Grid where
  traverse f (Grid dims elems) = Grid dims <$> traverse f elems
