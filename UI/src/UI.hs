{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- module UI where

import Types
import ClassyPrelude
import Reflex
import Reflex.Dom
import Data.Default
import Path
import Data.Aeson

pureButton :: MonadWidget t m => String -> m (Event t ())
pureButton label = do
  (e, _) <- elAttr' "button" ("class" =: "pure-button") (text label)
  return $ domEvent Click e

pureTextInput ph = textInput $ def
                   & attributes .~ (constDyn $ "placeholder" =: ph)

purePasswordInput ph = textInput $ def
                   & attributes .~ ( constDyn $ "placeholder" =: ph)
                   & textInputConfig_inputType .~ ("password")

logSettingsForm :: MonadWidget t m => m (Dynamic t (Either SomeException LogSettings), Event t ())
logSettingsForm = do
  d5 <- elAttr "form" ("class" =: "pure-form") $ do
    un <- mapDyn pack =<< value <$> pureTextInput "Username"
    el "br" blank
    pw <- mapDyn pack =<< value <$> purePasswordInput "Password"
    el "br" blank
    nodes <- mapDyn (fmap pack . splitElem ',') =<< value <$> pureTextInput "Nodes"
    el "br" blank
    lName <- mapDyn parseRelFile =<< value <$> pureTextInput "Logfile Name"
    el "br" blank
    hostUrl <- mapDyn pack =<< value <$> pureTextInput "Server URL"
    el "br" blank

    d1 <- combineDyn LogSettings un pw
    d2 <- combineDyn (\x y -> x y) d1 nodes
    d3 <- combineDyn (\x y -> x <$> y) d2 lName
    d4 <- combineDyn (\x y -> x <*> y) d3 (constDyn $ parseRelDir "logs/")
    combineDyn (\x y -> x <*> pure y) d4 hostUrl

  click <- pureButton "Submit"
  return (d5, click)

testXhr :: MonadWidget t m => Dynamic t LogSettings -> Event t () -> m ()
testXhr logSettings click = do
  let evt = fmap createRequest $ attachDyn logSettings (reqCfg <$ click)
      reqCfg = def { _xhrRequestConfig_headers = ("Content-type" =: "application/json")
                }
  dn <- count evt
  dyn =<< mapDyn (text . show :: MonadWidget t m => Int -> m ()) dn
  asyncReq <- performRequestAsync evt
  resp <- holdDyn Nothing $ fmap _xhrResponse_responseText asyncReq
  received <- mapDyn (fmap asText . join . fmap decodeText) resp
  el "br" blank
  el "div" $ dyn =<< mapDyn ((text . show)) received
  blank

createRequest :: (LogSettings, XhrRequestConfig) -> XhrRequest
createRequest (info, cfg) = xhrRequest "POST" "http://localhost:8081/submit" cfg'
  where
    cfg' = cfg { _xhrRequestConfig_sendData = Just . unpack . decodeUtf8 . toStrict $ encode info }

doIt :: MonadWidget t m => m ()
doIt = do
  (eSettings, click) <- logSettingsForm
  dynText =<< mapDyn show eSettings
  logSettings <- mapDynM handleMSettings eSettings
  testXhr logSettings click

handleMSettings :: MonadSample t m => Either SomeException LogSettings -> m LogSettings
handleMSettings (Left e) = fail $ show e
handleMSettings (Right logSettings) = return logSettings

head :: MonadWidget t m => m ()
head = do
  elAttr "link" ( "rel" =: "stylesheet"
               <> "href" =: "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"
                ) blank

main :: IO ()
main = mainWidgetWithHead Main.head doIt
