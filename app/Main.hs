{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude hiding ((</>))
import MachineUtils hiding (run)
import GetLogs
import Network.HTTP.Client.TLS
import Network.HTTP.Client
import Control.Monad.Trans.Resource
import System.IO (hGetEcho, hSetEcho, hFlush, putChar)
import Path
import Server
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  (serverUrl:logNameIn:logDestIn:_) <- getArgs
  logName <- parseRelFile $ unpack logNameIn
  logDest <- parseRelDir $ unpack logDestIn
  logSettings <- getUserInput serverUrl logName logDest
  testIt logSettings

getUserInput :: MonadIO m => Text -> Path Rel File -> Path Rel Dir -> m LogSettings
getUserInput serverUrl logName logDest =
  LogSettings <$> getInputLine "username: "
              <*> getPassword "password: "
              <*> pure nodes
              <*> pure logName
              <*> pure logDest
              <*> pure serverUrl

testIt :: LogSettings -> IO ()
testIt logSettings = do
  mgr <- newManager tlsManagerSettings
  req <- parseUrlThrow url
  mapM_ (\node -> runRMachine_ (getNode (encodeUtf8 node) >>> login url mgr un pw >>> downloadLog logUrlBase (pack $ fromRelFile log) mgr >>> sourceHttp_ >>> downloadHttp (saveName $ unpack node) >>> tee) [(req, mgr)]) nodes
    where
      nodes = nodeNames logSettings
      log = logName logSettings
      dest = logDestination logSettings
      un = username logSettings
      pw = password logSettings
      saveName node = fromRelFile (dest </> filename log) <> "." <> node
      url = unpack $ logUrl logSettings
      logUrlBase = url <> "/suite/logs"

getInputLine :: MonadIO m => String -> m Text
getInputLine prompt = liftIO $ do
  putStr $ pack prompt
  hFlush stdout
  getLine >>= return . pack

getPassword :: MonadIO m => String -> m Text
getPassword prompt = liftIO $ do
  putStr $ pack prompt
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return $ pack pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

data LogSettings = LogSettings
  { username :: Text
  , password :: Text
  , nodeNames :: [Text]
  , logName :: Path Rel File
  , logDestination :: Path Rel Dir
  , logUrl :: Text
  } deriving Show

nodes = ["node1873", "node1894", "node1895", "node1896", "node1897", "node1898", "node1899", "node1900"]
