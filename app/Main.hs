{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Oracle.Data.Embeddable
import Oracle.Control.Monad.Search
import Oracle.Neural

import Data.ByteString.UTF8 as BSU

import Network.Simple.TCP
import System.IO


import Data.ProtoLens (defMessage, encodeMessage, decodeMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro
import Lens.Micro.Extras (view)

import qualified Proto.Embeddable as P
import qualified Proto.ChoicePoint as P
import qualified Proto.DataPoint as P
import qualified Proto.Command as P
import qualified Proto.Response as P

main :: IO ()
main = do
  let snapshot :: P.Embeddable = toProto . toEmbeddable $ [("k1", 1 :: Int), ("k2", 2)]
  let choices :: [P.Embeddable] = map (toProto . toEmbeddable) [
        [("c1a", True ), ("c1b", False)],
        [("c2a", False), ("c2b", True )]
        ]
  let choicePoint :: P.ChoicePoint = defMessage & #snapshot .~ snapshot & #choices .~ choices
  let simpleCmd :: P.Command       = defMessage & #predict .~ (defMessage & #choicePoints .~ [choicePoint])

  connect "localhost" "10000" $ \(sock, remoteAddr) -> do
    putStrLn $ "Connection established to " ++ show remoteAddr
    send sock $ encodeMessage simpleCmd
    msg <- recv sock 1000
    case msg of
      Nothing -> putStrLn "recv failed"
      Just msg -> print $ (decodeMessage msg :: Either String P.Response)
