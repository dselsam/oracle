{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Oracle.Deque where

import Data.Sequence as Seq
import qualified Data.Sequence as Seq

data Deque a = Deque !(Seq a)
             deriving (Eq, Ord, Show)

empty :: Deque a
empty = Deque Seq.empty

data Side = Front | Back deriving (Eq, Ord, Show)

singleton :: a -> Deque a
singleton = Deque . Seq.singleton

popFront :: Deque a -> Maybe (a, Deque a)
popFront (Deque xs)
  | Seq.null xs = Nothing
  | otherwise   = Just (Seq.index xs 0, Deque (Seq.drop 1 xs))

push :: Side -> a -> Deque a -> Deque a
push side x deque = case side of
  Front -> pushFront x deque
  Back  -> pushBack x deque

pushFront :: a -> Deque a -> Deque a
pushFront x (Deque xs) = Deque (x <| xs)

pushBack :: a -> Deque a -> Deque a
pushBack x (Deque xs) = Deque (xs |> x)
