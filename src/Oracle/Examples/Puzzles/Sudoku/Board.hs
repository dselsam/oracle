{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Oracle.Examples.Puzzles.Sudoku.Board where

import Oracle.Data.Embeddable

import Oracle.Data.Grid (Grid(Grid), Index(Index))
import qualified Oracle.Data.Grid.Grid as Grid
import qualified Oracle.Data.Grid.Index as Index

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad (guard, when)
import Control.Applicative (Alternative)
import Control.Monad.State (MonadState, get, gets, modify)
import Control.Monad.Reader (MonadReader, ask, asks)
import Data.Foldable (for_)

newtype Value = Value Int deriving (Eq, Ord, Show)

instance HasToEmbeddable Value where
  toEmbeddable (Value x) = toEmbeddable x

data Board = Board {
  grid   :: Grid Value,
  empty  :: Set Index
  } deriving (Show)

instance HasToEmbeddable Board where
  toEmbeddable (Board grid empty) = ERecord "sudoku board" [
    ("grid",   toEmbeddable grid),
    ("empty",  toEmbeddable empty),
    ("nEmpty", toEmbeddable (Set.size empty))
    ]

isFilled :: (MonadState Board m) => m Bool
isFilled = gets $ Set.null . empty

-- TODO: awkward to have reader/state versions separate
readIdx :: (MonadReader Board m) => Index -> m Value
readIdx idx = do
  grid <- asks grid
  pure $ Grid.get idx grid

lookupIdx :: (MonadState Board m) => Index -> m Value
lookupIdx idx = do
  grid <- gets grid
  pure $ Grid.get idx grid

data SubgridIndex = SubgridIndex {
  outer :: Index,
  inner :: Index
  } deriving (Eq, Ord, Show)

idx2subgrid :: Index -> SubgridIndex
idx2subgrid (Index i j) = SubgridIndex {
  outer = Index (div i 3) (div j 3),
  inner = Index (mod i 3) (mod j 3)
  }

subgrid2idx :: SubgridIndex -> Index
subgrid2idx (SubgridIndex oidx iidx) = Index.scale 3 oidx + iidx

lookupSubgridIdx :: (MonadState Board m) => SubgridIndex -> m Value
lookupSubgridIdx sgIdx = do
  grid <- gets grid
  pure $ Grid.get (subgrid2idx sgIdx) grid

set :: (MonadState Board m, MonadFail m, Alternative m) => Index -> Value -> m ()
set idx x = do
  Value y <- lookupIdx idx
  when (y > 0) $ fail "position already occupied"
  check idx x
  modify $ \b -> b { grid = Grid.set idx x (grid b), empty = Set.delete idx (empty b) }

check :: (MonadState Board m, Alternative m) => Index -> Value -> m ()
check idx@(Index r c) x = do
  let sg = idx2subgrid idx
  for_ [0..8] $ \k -> do
    lookupIdx (Index r k) >>= \yc -> guard (yc /= x)
    lookupIdx (Index k c) >>= \yr -> guard (yr /= x)
    lookupSubgridIdx (sg { inner = Index (div k 3) (mod k 3) }) >>= \ys -> guard (ys /= x)
