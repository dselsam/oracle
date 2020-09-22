{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Train-test split (TTS).
-}

{-# LANGUAGE ScopedTypeVariables #-}
module Oracle.Examples.Synth.TTS where

import Oracle.Data.Embeddable
import Oracle.Examples.Synth.TTSInfo
import qualified Data.List as List
import qualified Oracle.Util.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

type ForTrain a = [a]
type ForTest a  = [a]

data TTS a = TTS {
  train :: ForTrain a,
  test  :: ForTest a
  } deriving (Eq, Ord, Show)

instance Functor TTS where
  fmap f (TTS train test) = TTS (fmap f train) (fmap f test)

instance Foldable TTS where
  foldMap f (TTS train test) = foldMap f train <> foldMap f test

instance Traversable TTS where
  traverse f (TTS train test) = TTS <$> traverse f train <*> traverse f test

getInfo   (TTS train test) = TTSInfo (length train) (length test)

toBigList (TTS train test) = train ++ test
fromBigList xs (TTSInfo nTrain nTest) = TTS (take nTrain xs) (drop nTrain xs)

replicate (TTSInfo nTrain nTest) a = TTS (List.replicate nTrain a) (List.replicate nTest a)

replicateLikeNested :: TTS [a] -> TTS b -> TTS b
replicateLikeNested xss ys = TTS (concatMap (\(xs, y) -> List.replicate (length xs) y) (Prelude.zip (train xss) (train ys)))
                                (concatMap (\(xs, y) -> List.replicate (length xs) y) (Prelude.zip (test xss) (test ys)))

-- Note: all unnecessary, but useful to keep lists-vs-ex straight
map  :: (a -> b) -> TTS a -> TTS b
map f ex = fmap f ex

mapM  :: (Monad m) => (a -> m b) -> TTS a -> m (TTS b)
mapM = Traversable.mapM

all :: (a -> Bool) -> TTS a -> Bool
all = Foldable.all

any :: (a -> Bool) -> TTS a -> Bool
any = Foldable.any

zip     (TTS t1 u1) (TTS t2 u2) = TTS (Prelude.zip t1 t2) (Prelude.zip u1 u2)
zip3    (TTS t1 u1) (TTS t2 u2) (TTS t3 u3) = TTS (Prelude.zip3 t1 t2 t3) (Prelude.zip3 u1 u2 u3)
zip4    (TTS t1 u1) (TTS t2 u2) (TTS t3 u3) (TTS t4 u4) = TTS (List.zip4 t1 t2 t3 t4) (List.zip4 u1 u2 u3 u4)

unzip :: TTS (a, b) -> (TTS a, TTS b)
unzip (TTS xyTrains xyTests) =
  let (xTrains, yTrains) = List.unzip xyTrains
      (xTests,  yTests)  = List.unzip xyTests
  in
    (TTS xTrains xTests, TTS yTrains yTests)

unzip3 :: TTS (a, b, c) -> (TTS a, TTS b, TTS c)
unzip3 (TTS xyzTrains xyzTests) =
  let (xTrains, yTrains, zTrains) = List.unzip3 xyzTrains
      (xTests,  yTests, zTests)  = List.unzip3 xyzTests
  in
    (TTS xTrains xTests, TTS yTrains yTests, TTS zTrains zTests)

unzip4 :: TTS (a, b, c, d) -> (TTS a, TTS b, TTS c, TTS d)
unzip4 (TTS xyzwTrains xyzwTests) =
  let (xTrains, yTrains, zTrains, wTrains) = List.unzip4 xyzwTrains
      (xTests,  yTests, zTests, wTests)  = List.unzip4 xyzwTests
  in
    (TTS xTrains xTests, TTS yTrains yTests, TTS zTrains zTests, TTS wTrains wTests)

sequence :: TTS [a] -> [TTS a]
sequence (TTS trainss testss) =
  [TTS train test | train <- Traversable.sequence trainss, test <- Traversable.sequence testss]

pullList :: TTS [a] -> [TTS a]
pullList (TTS (trains :: [[a]]) (tests :: [[a]])) =
  flip Prelude.map (Prelude.zip trains tests) $ uncurry TTS

pushList :: [TTS a] -> TTS [a]
pushList exs = TTS (Prelude.map train exs) (Prelude.map test exs)

flatten :: TTS [a] -> (TTS a, TTS b -> TTS [b])
flatten xss@(TTS trains tests) = (TTS (concat trains) (concat tests), unflattenLike xss)

unflattenLike :: TTS [a] -> TTS b -> TTS [b]
unflattenLike xss ys = TTS (List.unflattenLike (train xss) $ train ys) (List.unflattenLike (test xss) $ test ys)

flatProvenance :: TTS [a] -> TTS Int
flatProvenance x = TTS (concatMap (\(i, xs) -> List.replicate (length xs) i) (Prelude.zip [0..] (train x)))
                      (concatMap (\(i, xs) -> List.replicate (length xs) (i + length (train x))) (Prelude.zip [0..] (test x)))

partitionOn :: TTS Bool -> TTS a -> (TTS a, TTS a)
partitionOn bs xs =
  let (trainTrues, trainFalses) = List.partitionOn (train bs) (train xs)
      (testTrues,  testFalses)  = List.partitionOn (test bs)  (test xs)
  in
    (TTS trainTrues testTrues, TTS trainFalses testFalses)

unpartitionOn :: TTS Bool -> TTS a -> TTS a -> TTS a
unpartitionOn bs xs ys = TTS (List.unpartitionOn (train bs) (train xs) (train ys))
                             (List.unpartitionOn (test  bs) (test  xs) (test  ys))

partitionInfosOn :: TTS Bool -> (TTSInfo, TTSInfo)
partitionInfosOn bs = (TTSInfo (length $ filter id (train bs))  (length $ filter id (test bs)),
                       TTSInfo (length $ filter not (train bs)) (length $ filter not (test bs)))

instance (HasToEmbeddable a) => HasToEmbeddable (TTS a) where
  toEmbeddable (TTS train test) = ERecord "TTS" [
    ("train", toEmbeddable train),
    ("test",  toEmbeddable test)
    ]
