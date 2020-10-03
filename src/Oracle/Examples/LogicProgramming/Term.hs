{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

-}

module Oracle.Examples.LogicProgramming.Term where

import Oracle.Examples.LogicProgramming.Var
import Oracle.Examples.LogicProgramming.Op

import Data.Foldable (asum)

data Term = TVar Var
          | App Op [Term]
          deriving (Eq, Ord, Show)

find :: (Term -> Bool) -> Term -> Maybe Term
find h t
  | h t       = Just t
  | otherwise = case t of
      App _ args  -> asum $ map (find h) args
      _           -> Nothing

replace :: (Term -> Maybe Term) -> Term -> Term
replace h t = case h t of
  Just u  -> u
  Nothing -> case t of
    App f args -> App f (map (replace h) args)
    _          -> t
