{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Server where

import ClassyPrelude hiding (Handler, (</>))

import Data.Aeson
import Servant
import Types
import Lucid
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Path
import GetLogs (downloadLogs)
import MachineUtils

type HomePageAPI = Get '[HTMLLucid] HomePage
type SubmitAPI = "submit" :> ReqBody '[JSON] LogSettings :> Post '[JSON] SubmitStatus
type ScriptsAPI = "scripts" :> Capture "fileName" (Path Rel File) :> Get '[Javascript] BL.ByteString

type API = HomePageAPI
  :<|> SubmitAPI
  :<|> ScriptsAPI

data HomePage = HomePage
  { scripts :: [Text]
  } deriving Generic

data HTMLLucid

instance Accept HTMLLucid where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTMLLucid a where
  mimeRender _ = renderBS . toHtml

instance MimeRender HTMLLucid (Html a) where
  mimeRender _ = renderBS

data Javascript

instance Accept Javascript where
  contentType _ = "application" // "javascript"

instance MimeRender Javascript BL.ByteString where
  mimeRender _ = id

instance ToHtml HomePage where
  toHtml page = do
    html_ $ head_ $
      mapM_ (\src -> script_ [lang_ "javascript", src_ src] (mempty :: Text)) $ scripts page
    body_ ""
    case parseRelFile "runmain.js" of
      Nothing -> return ()
      Just fn -> script_ [lang_ "javascript", src_ (tshow $ scriptLink fn), defer_ ("" :: Text)] ("" :: Text)

  toHtmlRaw = toHtml

instance FromHttpApiData (Path Rel File) where
  parseUrlPiece t = case parseRelFile (unpack t) of
    Left e -> Left (tshow e)
    Right v -> Right v
  parseHeader _ = Left "Not implemented"
  parseQueryParam _ = Left "Not implemented"

instance ToHttpApiData (Path Rel File) where
  toUrlPiece = pack . fromRelFile
  toHeader _ = mempty
  toQueryParam _ = mempty

proxyAPI :: Proxy API
proxyAPI = Proxy

homepage = HomePage $ fmap (tshow . scriptLink) [rtsJS, libJS, outJS]

checkLogSettings :: LogSettings -> Handler SubmitStatus
checkLogSettings logSettings = do
  liftIO $ do
    print $ "Received: " <> tshow logSettings
    print "Downloading logs now"
    res <- tryAny $ downloadLogs logSettings
    case res of
      Left e -> do
        print e
        return $ SubmissionError $ tshow e
      Right () -> do
        putStrLn "Successfully downloaded the logs"
        -- return $ Confirmation "Successfully downloaded the logs"
        mDirContents <- liftIO $ mapM (\dest -> runRMachine (sourceDirectory >>> evMap (asText . pack)) [fromRelDir dest]) $ logDestination logSettings
        case mDirContents of
          Nothing -> return $ SubmissionError "Could not get the log files"
          Just dirContents -> return $ Confirmation dirContents

serveFile :: Path Rel Dir -> Path Rel File -> Handler BL.ByteString
serveFile scriptsDir fName = do
  eBS <- tryAny . readFile . fromRelFile $ scriptsDir </> fName
  case eBS of
    Left _ -> return "Resource not found"
    Right bs -> return bs

server :: Path Rel Dir -> Server API
server scriptsDir = return homepage
  :<|> checkLogSettings
  :<|> serveFile scriptsDir

scriptLink :: Path Rel File -> URI
scriptLink = safeLink proxyAPI scpt
  where
    scpt = Proxy :: Proxy ScriptsAPI

app ::Path Rel Dir -> Application
app scriptsDir = serve proxyAPI (server scriptsDir)

rtsJS = $(mkRelFile "rts.js")
libJS = $(mkRelFile "lib.js")
outJS = $(mkRelFile "out.js")
