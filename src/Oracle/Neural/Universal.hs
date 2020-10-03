{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
module Oracle.Neural.Universal where

import qualified Oracle.Data.Embeddable as Embeddable
import Oracle.Control.Monad.Search (ChoicePoint(ChoicePoint))
import Oracle.Search.Decision (Decision(Decision))

import Network.Simple.TCP
import System.IO

import Data.ProtoLens (defMessage, encodeMessage, decodeMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro
import Lens.Micro.Extras (view)

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Text as Text

import qualified Proto.Protos.ChoicePoint as P
import qualified Proto.Protos.Result as P
import qualified Proto.Protos.Label as P
import qualified Proto.Protos.DataPoint as P
import qualified Proto.Protos.Command as P
import qualified Proto.Protos.Response as P

init :: P.Command
init = defMessage & #init .~ defMessage

predict :: [ChoicePoint m a] -> P.Command
predict choicePoints = defMessage
  & #predict .~ (defMessage
                 & #choicePoints .~ (flip map choicePoints $ \(ChoicePoint snap cs) ->
                                        defMessage
                                        & #snapshot .~ Embeddable.toProto snap
                                        & #choices  .~ map (Embeddable.toProto . fst) (Vector.toList cs)))

train :: Int -> [Decision] -> P.Command
train nEpochs decisions = defMessage
  & #train .~ (defMessage
                 & #nEpochs .~ (fromIntegral nEpochs)
                 & #datapoints .~ (flip map decisions $ \(Decision snap choices choiceIdx) ->
                                      defMessage
                                      & #choicePoint .~ (defMessage
                                                         & #snapshot .~ Embeddable.toProto snap
                                                         & #choices  .~ Vector.toList (fmap Embeddable.toProto choices))
                                      & #label .~ (defMessage
                                                  & #choiceIdx  .~ (fromIntegral choiceIdx))))

save :: FilePath -> P.Command
save filename = defMessage
  & #save .~ (defMessage
              & #filename .~ Text.pack filename)

load :: FilePath -> P.Command
load filename = defMessage
  & #load .~ (defMessage
              & #filename .~ Text.pack filename)

execute :: String -> String -> P.Command -> IO P.Response
execute host port cmd = connect host port $ \(sock, remoteAddr) -> do
  send sock $ encodeMessage cmd
  msg <- recv sock 10000
  case msg of
    Nothing  -> error $ "[execute] recv returned Nothing"
    Just msg -> case decodeMessage msg of
      Left err       -> error $ "[execute] recv returned Left: " ++ err
      Right response -> pure response
