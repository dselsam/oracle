{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Oracle.Data.Embeddable
import Oracle.SearchT
import Oracle.Neural

import Data.ByteString.UTF8 as BSU

import Network.Simple.TCP
import System.IO

import Data.ProtoLens (defMessage, encodeMessage, decodeMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro
import Lens.Micro.Extras (view)

import qualified Proto.Protos.Embeddable as P
import qualified Proto.Protos.ChoicePoint as P
import qualified Proto.Protos.Result as P
import qualified Proto.Protos.DataPoint as P
import qualified Proto.Protos.Command as P

main :: IO ()
main = do
  let snapshot :: P.Embeddable = toProto . toEmbeddable $ [("k1", 1 :: Int), ("k2", 2)]
  let choices :: [P.Embeddable] = map (toProto . toEmbeddable) [
        [("c1a", True ), ("c1b", False)],
        [("c2a", False), ("c2b", True )]
        ]
  let choicePoint :: P.ChoicePoint = defMessage & #snapshot .~ snapshot & #choices .~ choices
  let simpleCmd :: P.Command       = defMessage & #predict .~ (defMessage & #choicepoint .~ choicePoint)

  connect "localhost" "10000" $ \(sock, remoteAddr) -> do
    putStrLn $ "Connection established to " ++ show remoteAddr
    send sock $ encodeMessage simpleCmd
    msg <- recv sock 1000
    case msg of
      Nothing -> putStrLn "recv failed"
      Just msg -> print $ (decodeMessage msg :: Either String P.Command)
