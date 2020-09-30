{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

-}

module Oracle.Examples.LogicProgramming.Var where

newtype Var = Var Int deriving (Eq, Ord, Show)
