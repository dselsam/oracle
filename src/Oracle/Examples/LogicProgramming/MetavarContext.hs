{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

-}
{-# LANGUAGE LambdaCase #-}
module Oracle.Examples.LogicProgramming.MetavarContext where

import Oracle.Examples.LogicProgramming.Var (Var(Var))
import Oracle.Examples.LogicProgramming.Op (Op(Op))
import Oracle.Examples.LogicProgramming.Term (Term(..))
import Oracle.Examples.LogicProgramming.Rule (Rule(..))

import qualified Oracle.Examples.LogicProgramming.Term as Term
import qualified Oracle.Examples.LogicProgramming.Rule as Rule

import Control.Monad.Extra (whenM, unlessM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, runState, get, gets, modify)
import Data.Foldable (for_)
import Data.Maybe (isJust, isNothing)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq

import qualified Data.List as List

data MetavarContext = MetavarContext {
  id2term :: Seq (Maybe Term)
  } deriving (Eq, Ord, Show)

type MCM a = ExceptT String (State MetavarContext) a

empty :: MetavarContext
empty = MetavarContext Seq.empty

newMetas :: Int -> MCM [Term]
newMetas k = do
  start <- gets $ Seq.length . id2term
  modify $ \mctx -> mctx { id2term = id2term mctx >< Seq.replicate k Nothing }
  pure $ map (TVar . Var) [start..start+k-1]

getVar :: Term -> Maybe Var
getVar t =
  case t of
    TVar var -> Just var
    _ -> Nothing

isVar :: Term -> Bool
isVar = isJust . getVar

lookupVar :: Var -> MCM (Maybe Term)
lookupVar (Var i) = gets $ \mctx -> Seq.index (id2term mctx) i

lookupTerm :: Term -> MCM (Maybe Term)
lookupTerm (TVar var) = lookupVar var

isAssigned :: Term -> MCM Bool
isAssigned t = fmap isJust $ lookupTerm t

assignVar :: Var -> Term -> MCM ()
assignVar var@(Var i) t = do
  unlessM (fmap isNothing $ lookupVar var) $ error $ "[assignVar] overwriting assignment"
  modify $ \mctx -> mctx { id2term = Seq.update i (Just t) (id2term mctx) }

assignTerm :: Term -> Term -> MCM ()
assignTerm (TVar var) = assignVar var

shallowInstantiate :: Term -> MCM Term
shallowInstantiate t =
  case t of
    TVar var -> do
      result <- lookupVar var
      case result of
        Just u -> shallowInstantiate u
        Nothing -> pure t
    _ -> pure t

match :: Term -> Term -> MCM ()
match pat t = do
  pat <- shallowInstantiate pat
  if pat == t then pure () else
    case (pat, t) of
      (TVar var, _) -> assignVar var t
      (App f1 ts1, App f2 ts2)
        | f1 == f2 && length ts1 == length ts2 -> for_ (zip ts1 ts2) (uncurry match)
      (_, _)
        | otherwise -> throwError $ "match failed: " ++ show pat ++ " ?= " ++ show t

unify :: Term -> Term -> MCM ()
unify t1 t2 = do
  t1 <- shallowInstantiate t1
  t2 <- shallowInstantiate t2
  case (t1, t2) of
    (TVar var1, TVar var2) | var1 == var2 -> pure ()
    (TVar var, _)
      | not (occurs t1 t2) -> assignVar var t2
      | otherwise -> throwError $ "occurs check failed: " ++ show t1 ++ " occurs in " ++ show t2
    (_, TVar _) -> unify t2 t1
    (App f1 ts1, App f2 ts2)
      | f1 == f2 && length ts1 == length ts2 -> for_ (zip ts1 ts2) (uncurry unify)
    (_, _)
      | t1 == t2  -> pure ()
      | otherwise -> throwError $ "unify failed: " ++ show t1 ++ " =?= " ++ show t2
  where
    occurs :: Term -> Term -> Bool
    occurs t1 t2 = isJust $ Term.find (==t1) t2

instantiate :: Term -> MCM Term
instantiate t = do
  case t of
    App f ts -> App f <$> mapM instantiate ts
    TVar var -> lookupVar var >>= \case
      Nothing -> pure t
      Just u  -> instantiate u
