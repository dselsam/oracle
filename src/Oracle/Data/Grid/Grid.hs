module Oracle.Data.Grid.Grid where

import Control.Monad (guard)
import Control.Monad.Identity (Identity, runIdentity)

import Oracle.Data.Grid.Index (Index(..))
import qualified Oracle.Data.Grid.Index as Index

import Oracle.Data.Grid.Dims (Dims(..))
import qualified Oracle.Data.Grid.Dims as Dims

import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Grid a = Grid {
  dims    :: Dims,
  elems   :: Vector a
  } deriving (Eq, Ord, Show)

fromArray :: Dims -> Vector a -> Grid a
fromArray dims v | Dims.nCells dims == Vector.length v = Grid dims v

fromList :: Dims -> [a] -> Grid a
fromList dims elems = fromArray dims (Vector.fromList elems)

fromLists :: [[a]] -> Grid a
fromLists [] = Grid (Dims 0 0) (Vector.fromList [])
fromLists elems =
  let m = length elems
      n = length (head elems) in
    fromArray (Dims m n) (Vector.fromList $ concat elems)

fromIntLists :: [[Int]] -> Grid Int
fromIntLists elems = fromLists elems

-- TODO: duplication
fromFuncM :: (Monad m) => Dims -> (Index -> m a) -> m (Grid a)
fromFuncM dims@(Dims m n) f = do
  elems <- Vector.generateM (m*n) $ \i -> f (Dims.int2index dims i)
  pure $ fromArray dims elems

fromFunc :: Dims -> (Index -> a) -> Grid a
fromFunc dims f = runIdentity $ fromFuncM dims (pure . f)

toOffset :: Grid a -> Index -> Int
toOffset g (Index i j) = i * Dims.nCols (dims g) + j

toOffsetSafe :: Grid a -> Index -> Maybe Int
toOffsetSafe g idx = do
  guard $ Dims.inBounds (dims g) idx
  pure $ toOffset g idx

at :: Index -> Grid a -> a
at idx g = elems g Vector.! (toOffset g idx)

mapM :: (Monad m) => (Index -> a -> m b) -> Grid a -> m (Grid b)
mapM f g = fromFuncM (dims g) $ \idx -> f idx $ at idx g

map :: (Index -> a -> b) -> Grid a -> Grid b
map f g = fromFunc (dims g) $ \idx -> f idx $ at idx g
