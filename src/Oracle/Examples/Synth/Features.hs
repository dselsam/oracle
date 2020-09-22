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

import Oracle.Data.Embeddable
import Oracle.SearchT (NamedChoice)
import Oracle.Examples.Synth.TTSInfo (TTSInfo)
import Oracle.Examples.Synth.TTS (TTS)
import qualified Oracle.Examples.Synth.TTS as TTS
import qualified Oracle.Util.List as List

import Oracle.Examples.Synth.SynthContext

import qualified Data.List as List

newtype Features a = Features {
  choices :: [NamedChoice (TTS a)]
  } deriving (Eq, Ord, Show)

isEmpty :: Features a -> Bool
isEmpty = List.null . choices

getInfo :: Features a -> TTSInfo
getInfo = TTS.getInfo . getTTS 0

getTTS :: Int -> Features a -> TTS a
getTTS i (Features features) = snd $ features List.!! i

instance SynthContext (Features a) where
  partitionOn bs (Features choices) =
    let bothChoices :: [NamedChoice (TTS a, TTS a)] = map (\(name, ex) -> (name, TTS.partitionOn bs ex)) choices in
      (Features $ map (\(name, (ex1, _)) -> (name, ex1)) bothChoices,
       Features $ map (\(name, (_, ex2)) -> (name, ex2)) bothChoices)

instance SynthContextFlatten (Features [a]) (Features a) where
  flattenCtx (Features choices) = Features $ flip map choices $ \(n, ex) -> (n, fst (TTS.flatten ex))

instance (Eq a, Ord a) => SynthContextSelect (Features [a]) (Features a) where
  selectCtx bs (Features choices) = do
    let keeps = flip concatMap choices $ \(n, xs) ->
          case selectCtx bs xs of
            Nothing -> []
            Just x -> [(n, x)]
    pure $ Features keeps

append :: Features a -> [NamedChoice (TTS a)] -> Features a
append efs cs = Features (choices efs ++ cs)

prepend :: Features a -> [NamedChoice (TTS a)] -> Features a
prepend efs cs = Features (cs ++ choices efs)

instance (HasToEmbeddable a) => HasToEmbeddable (Features a) where
  toEmbeddable (Features choices) = ERecord "Features" [
    ("choices", toEmbeddable choices)
    ]
