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

import ClassyPrelude hiding (Handler)

import Data.Aeson
import Servant
import Types
import Lucid
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Path

type HomePageAPI = Get '[HTMLLucid] HomePage
type SubmitAPI = "submit" :> ReqBody '[JSON] LogSettings :> Post '[JSON] Confirmation
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

checkLogSettings :: LogSettings -> Handler Confirmation
checkLogSettings logSettings = return $ Confirmation $ msg log
  where
    msg (Just lName) = "Received request for " <> pack (fromRelFile lName) <> " from " <> url
    msg Nothing = "Invalid logFile name!"
    log = logName logSettings
    url = logUrl logSettings

serveFile :: Path Rel File -> Handler BL.ByteString
serveFile fName = do
  eBS <- tryAny . readFile $ "UI.jsexe/" <> fromRelFile fName
  case eBS of
    Left _ -> return "Resource not found"
    Right bs -> return bs

server :: Server API
server = return homepage
  :<|> checkLogSettings
  :<|> serveFile

scriptLink :: Path Rel File -> URI
scriptLink = safeLink proxyAPI scpt
  where
    scpt = Proxy :: Proxy ScriptsAPI

app :: Application
app = serve proxyAPI server

rtsJS = $(mkRelFile "rts.js")
libJS = $(mkRelFile "lib.js")
outJS = $(mkRelFile "out.js")
