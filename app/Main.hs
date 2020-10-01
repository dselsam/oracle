module Main where

import Oracle.Data.Embeddable
import Oracle.SearchT
import Oracle.Neural

import Data.ByteString.UTF8 as BSU

import Network.Simple.TCP
import System.IO

main :: IO ()
main = connect "localhost" "10000" $ \(sock, remoteAddr) -> do
  putStrLn $ "Connection established to " ++ show remoteAddr
  send sock $ BSU.fromString "save-weights myfilename"
  msg <- recv sock 1000
  print $ fmap BSU.toString msg
