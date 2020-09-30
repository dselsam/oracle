{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

-}

module Oracle.Examples.LogicProgramming.Rule where

import Oracle.Examples.LogicProgramming.Var
import Oracle.Examples.LogicProgramming.Op
import Oracle.Examples.LogicProgramming.Term

-- Terms inside rules will use [Var 0, ..., Var (nFree-1)] to denote the free variables.
-- Rules will need to be internalized into metavar contexts.
data Rule = Rule {
  name       :: String,
  nFree      :: Int,
  hyps       :: [Term],
  conclusion :: Term
  } deriving (Eq, Ord, Show)
