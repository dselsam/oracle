{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

This file contains a simplistic baseline `Embeddable -> [Token]`
for use with seq2seq models.
-}

module Oracle.Neural.Sequence where

import Oracle.Data.Embeddable (Embeddable(..))

import Data.Sequence (Seq, (|>), (><))
import qualified Data.Sequence as Seq

import GHC.Exts (toList)
import qualified Data.List as List

type Token = String

embed :: Embeddable -> Seq Token
embed x = case x of
  EUnit u           -> Seq.singleton (show u)
  EBool b           -> Seq.singleton (show b)
  EInt k            -> embedInt k
  EString s         -> Seq.singleton s
  EPair (x, y)      -> Seq.fromList ["(", "Prod.mk"] >< embed x >< Seq.singleton "," >< embed y >< Seq.singleton "]"
  EList xs          -> foldl (\tokens x -> tokens >< embed x >< Seq.singleton ",") (Seq.singleton "[") xs |> "]"
  ESeq xs           -> foldl (\tokens x -> tokens >< embed x >< Seq.singleton ",") (Seq.fromList ["(", "Seq.mk"]) xs |> ")"
  ERecord n fields  -> foldl (\tokens (n, s) -> tokens >< embedField n s) (Seq.fromList ["(", "Record.mk", n]) fields |> ")"
  _                 -> error $ "[Oracle.Neural.Sequence.embed: missing several cases, cannot handle " ++ show x

  where
    embedField n s = Seq.fromList [":", n] >< embed s
    embedInt k
      | k < 0     = Seq.singleton "-" >< embedInt (- k)
      | otherwise = Seq.fromList $ map (\c -> [c]) (show k)


appendTokensToFile :: FilePath -> Seq Token -> IO ()
appendTokensToFile filename tokens = appendFile filename $ List.intercalate " " (toList tokens) ++ "\n"
