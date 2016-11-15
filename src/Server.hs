{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

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

type API = Get '[HTMLLucid] HomePage
  :<|> "submit" :> ReqBody '[JSON] LogSettings :> Post '[JSON] Confirmation

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
    script_ [lang_ "javascript", src_ "http://10.0.1.242:8081/scripts/runmain.js", defer_ ("" :: Text)] ("" :: Text)

  toHtmlRaw = toHtml

homeAPI :: Proxy API
homeAPI = Proxy

homepage = HomePage $ fmap (\x -> "http://10.0.1.242:8081/scripts/" <> x) ["rts.js", "lib.js", "out.js"]

checkLogSettings :: LogSettings -> Handler Confirmation
checkLogSettings logSettings = return $ Confirmation msg
  where
    msg = "Received " <> tshow logSettings

server :: Server API
server = return homepage
  :<|> checkLogSettings

app :: Application
app = serve homeAPI server
