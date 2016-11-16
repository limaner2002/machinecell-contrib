{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude hiding ((</>))
import Server
import Network.Wai.Handler.Warp (run)
import Path

main :: IO ()
main = do
  (scriptsDirIn:_) <- getArgs
  scriptsDir <- parseRelDir $ unpack scriptsDirIn
  run 8081 (app scriptsDir)
