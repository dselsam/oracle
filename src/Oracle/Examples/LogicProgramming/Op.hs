{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

-}

module Oracle.Examples.LogicProgramming.Op where

newtype Op = Op String deriving (Eq, Ord, Show)
