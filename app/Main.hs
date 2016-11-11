{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude hiding ((</>))
import MachineUtils
import GetLogs
import Network.HTTP.Client.TLS
import Network.HTTP.Client
import Control.Monad.Trans.Resource
import System.IO (hGetEcho, hSetEcho, hFlush, putChar)
import Path

main :: IO ()
main = do
  (serverUrl:logNameIn:logDestIn:_) <- getArgs
  logName <- parseRelFile $ unpack logNameIn
  logDest <- parseRelDir $ unpack logDestIn
  logSettings <- getUserInput serverUrl logName logDest
  -- print logSettings
  testIt logSettings

getUserInput :: MonadIO m => Text -> Path Rel File -> Path Rel Dir -> m LogSettings
getUserInput serverUrl logName logDest =
  LogSettings <$> getInputLine "username: "
              <*> getPassword "password: "
              <*> pure nodes -- getInputLine "node name to download from: "
              <*> pure logName -- getInputLine "logfile to download: "
              <*> pure logDest -- (unpack <$> getInputLine "destination to save logfile to: ")
              <*> pure serverUrl

testIt :: LogSettings -> IO ()
testIt logSettings = do
  mgr <- newManager tlsManagerSettings
  req <- parseUrlThrow url
  mapM_ (\node -> runRMachine_ (getNode (encodeUtf8 node) >>> login mgr un pw >>> downloadLog (pack $ fromRelFile log) mgr >>> sourceHttp_ >>> downloadHttp (saveName $ unpack node) >>> tee) [(req, mgr)]) nodes
    where
      nodes = nodeNames logSettings
      log = logName logSettings
      dest = logDestination logSettings
      un = username logSettings
      pw = password logSettings
      saveName node = fromRelFile (dest </> filename log) <> "." <> node
      url = unpack $ logUrl logSettings

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
