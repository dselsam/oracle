{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Inductive synthesis problems (ISP).
-}

{-# LANGUAGE ScopedTypeVariables #-}
module Oracle.Examples.Synth.ISP where

import Oracle.Examples.Synth.ISPInfo
import qualified Data.List as List
import qualified Oracle.Util.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

type ForTrain a = [a]
type ForTest a  = [a]

data ISP a = ISP {
  train :: ForTrain a,
  test  :: ForTest a
  } deriving (Eq, Ord, Show)

instance Functor ISP where
  fmap f (ISP train test) = ISP (fmap f train) (fmap f test)

instance Foldable ISP where
  foldMap f (ISP train test) = foldMap f train <> foldMap f test

instance Traversable ISP where
  traverse f (ISP train test) = ISP <$> traverse f train <*> traverse f test

getInfo   (ISP train test) = ISPInfo (length train) (length test)

toBigList (ISP train test) = train ++ test
fromBigList xs (ISPInfo nTrain nTest) = ISP (take nTrain xs) (drop nTrain xs)

replicate (ISPInfo nTrain nTest) a = ISP (List.replicate nTrain a) (List.replicate nTest a)

replicateLikeNested :: ISP [a] -> ISP b -> ISP b
replicateLikeNested xss ys = ISP (concatMap (\(xs, y) -> List.replicate (length xs) y) (Prelude.zip (train xss) (train ys)))
                                (concatMap (\(xs, y) -> List.replicate (length xs) y) (Prelude.zip (test xss) (test ys)))

-- Note: all unnecessary, but useful to keep lists-vs-ex straight
map  :: (a -> b) -> ISP a -> ISP b
map f ex = fmap f ex

mapM  :: (Monad m) => (a -> m b) -> ISP a -> m (ISP b)
mapM = Traversable.mapM

all :: (a -> Bool) -> ISP a -> Bool
all = Foldable.all

any :: (a -> Bool) -> ISP a -> Bool
any = Foldable.any

zip     (ISP t1 u1) (ISP t2 u2) = ISP (Prelude.zip t1 t2) (Prelude.zip u1 u2)
zip3    (ISP t1 u1) (ISP t2 u2) (ISP t3 u3) = ISP (Prelude.zip3 t1 t2 t3) (Prelude.zip3 u1 u2 u3)
zip4    (ISP t1 u1) (ISP t2 u2) (ISP t3 u3) (ISP t4 u4) = ISP (List.zip4 t1 t2 t3 t4) (List.zip4 u1 u2 u3 u4)

unzip :: ISP (a, b) -> (ISP a, ISP b)
unzip (ISP xyTrains xyTests) =
  let (xTrains, yTrains) = List.unzip xyTrains
      (xTests,  yTests)  = List.unzip xyTests
  in
    (ISP xTrains xTests, ISP yTrains yTests)

unzip3 :: ISP (a, b, c) -> (ISP a, ISP b, ISP c)
unzip3 (ISP xyzTrains xyzTests) =
  let (xTrains, yTrains, zTrains) = List.unzip3 xyzTrains
      (xTests,  yTests, zTests)  = List.unzip3 xyzTests
  in
    (ISP xTrains xTests, ISP yTrains yTests, ISP zTrains zTests)

unzip4 :: ISP (a, b, c, d) -> (ISP a, ISP b, ISP c, ISP d)
unzip4 (ISP xyzwTrains xyzwTests) =
  let (xTrains, yTrains, zTrains, wTrains) = List.unzip4 xyzwTrains
      (xTests,  yTests, zTests, wTests)  = List.unzip4 xyzwTests
  in
    (ISP xTrains xTests, ISP yTrains yTests, ISP zTrains zTests, ISP wTrains wTests)

sequence :: ISP [a] -> [ISP a]
sequence (ISP trainss testss) =
  [ISP train test | train <- Traversable.sequence trainss, test <- Traversable.sequence testss]

pullList :: ISP [a] -> [ISP a]
pullList (ISP (trains :: [[a]]) (tests :: [[a]])) =
  flip Prelude.map (Prelude.zip trains tests) $ uncurry ISP

pushList :: [ISP a] -> ISP [a]
pushList exs = ISP (Prelude.map train exs) (Prelude.map test exs)

flatten :: ISP [a] -> (ISP a, ISP b -> ISP [b])
flatten xss@(ISP trains tests) = (ISP (concat trains) (concat tests), unflattenLike xss)

unflattenLike :: ISP [a] -> ISP b -> ISP [b]
unflattenLike xss ys = ISP (List.unflattenLike (train xss) $ train ys) (List.unflattenLike (test xss) $ test ys)

flatProvenance :: ISP [a] -> ISP Int
flatProvenance x = ISP (concatMap (\(i, xs) -> List.replicate (length xs) i) (Prelude.zip [0..] (train x)))
                      (concatMap (\(i, xs) -> List.replicate (length xs) (i + length (train x))) (Prelude.zip [0..] (test x)))

partitionOn :: ISP Bool -> ISP a -> (ISP a, ISP a)
partitionOn bs xs =
  let (trainTrues, trainFalses) = List.partitionOn (train bs) (train xs)
      (testTrues,  testFalses)  = List.partitionOn (test bs)  (test xs)
  in
    (ISP trainTrues testTrues, ISP trainFalses testFalses)

unpartitionOn :: ISP Bool -> ISP a -> ISP a -> ISP a
unpartitionOn bs xs ys = ISP (List.unpartitionOn (train bs) (train xs) (train ys))
                             (List.unpartitionOn (test  bs) (test  xs) (test  ys))
