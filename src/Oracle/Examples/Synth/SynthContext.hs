{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Synthesis contexts.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
module Oracle.Examples.Synth.SynthContext where

import Oracle.Examples.Synth.TTS (TTS)
import qualified Oracle.Examples.Synth.TTS as TTS
import qualified Oracle.Util.List as List

class SynthContext ctx where
  partitionOn :: TTS Bool -> ctx -> (ctx, ctx)

instance SynthContext () where
  partitionOn _ _ = ((), ())

instance SynthContext (TTS a) where
  partitionOn = TTS.partitionOn

instance (SynthContext ctx1, SynthContext ctx2) => SynthContext (ctx1, ctx2) where
  partitionOn bs (ctx1, ctx2) =
    let (ctx1t, ctx1f) = partitionOn bs ctx1
        (ctx2t, ctx2f) = partitionOn bs ctx2
    in
      ((ctx1t, ctx2t), (ctx1f, ctx2f))

class SynthContextFlatten ctx1 ctx2 where
  flattenCtx :: ctx1 -> ctx2

instance SynthContextFlatten () () where
  flattenCtx _ = ()

instance SynthContextFlatten (TTS [a]) (TTS a) where
  flattenCtx ex = fst (TTS.flatten ex)

instance (SynthContextFlatten ctx1a ctx1b, SynthContextFlatten ctx2a ctx2b) => SynthContextFlatten (ctx1a, ctx2a) (ctx1b, ctx2b) where
  flattenCtx (ctx1a, ctx2a) = (flattenCtx ctx1a, flattenCtx ctx2a)

class SynthContextSelect ctx1 ctx2 where
  selectCtx :: TTS [Bool] -> ctx1 -> Maybe ctx2

instance SynthContextSelect () () where
  selectCtx _ _ = Just ()

instance (Eq a, Ord a) => SynthContextSelect (TTS [a]) (TTS a) where
  selectCtx bs xs = flip TTS.mapM (TTS.zip bs xs) $ \(bs, xs) ->
        List.getUnique id $ map snd . filter fst $ zip bs xs

instance (SynthContextSelect ctx1a ctx1b, SynthContextSelect ctx2a ctx2b) => SynthContextSelect (ctx1a, ctx2a) (ctx1b, ctx2b) where
  selectCtx bs (ctx1a, ctx2a) = do
    ctx1b <- selectCtx bs ctx1a
    ctx2b <- selectCtx bs ctx2a
    pure $ (ctx1b, ctx2b)
