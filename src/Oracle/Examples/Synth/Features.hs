{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Features, computed eagerly.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
module Oracle.Examples.Synth.Features where

import Oracle.SearchT (NamedChoice)
import Oracle.Examples.Synth.ISP (ISP)
import qualified Oracle.Examples.Synth.ISP as ISP
import qualified Oracle.Util.List as List

import Oracle.Examples.Synth.SynthContext

newtype Features a = Features {
  choices :: [NamedChoice (ISP a)]
  } deriving (Eq, Ord, Show)

instance SynthContext (Features a) where
  partitionOn bs (Features choices) =
    let bothChoices :: [NamedChoice (ISP a, ISP a)] = map (\(name, ex) -> (name, ISP.partitionOn bs ex)) choices in
      (Features $ map (\(name, (ex1, _)) -> (name, ex1)) bothChoices,
       Features $ map (\(name, (_, ex2)) -> (name, ex2)) bothChoices)

instance SynthContextFlatten (Features [a]) (Features a) where
  flattenCtx (Features choices) = Features $ flip map choices $ \(n, ex) -> (n, fst (ISP.flatten ex))

instance (Eq a, Ord a) => SynthContextSelect (Features [a]) (Features a) where
  selectCtx bs (Features choices) = do
    let keeps = flip concatMap choices $ \(n, xs) ->
          case selectCtx bs xs of
            Nothing -> []
            Just x -> [(n, x)]
    pure $ Features keeps

append :: Features a -> [NamedChoice (ISP a)] -> Features a
append efs cs = Features (choices efs ++ cs)

prepend :: Features a -> [NamedChoice (ISP a)] -> Features a
prepend efs cs = Features (cs ++ choices efs)
