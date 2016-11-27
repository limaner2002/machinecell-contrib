{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module UI where

import Types
import ClassyPrelude
import Reflex
import Reflex.Dom
import Data.Default
import Path
import Data.Aeson

newtype NothingYet = NothingYet Text
  deriving Show

instance Exception NothingYet

pureButton :: MonadWidget t m => String -> m (Event t ())
pureButton label = do
  (e, _) <- elAttr' "button" ("class" =: "pure-button") (text label)
  return $ domEvent Click e

pureTextInput :: MonadWidget t m => String -> Dynamic t (Map String String) -> m (TextInput t)
pureTextInput ph attrs = do
  let pureAttrs = constDyn $ "placeholder" =: ph
  dynAttrs <- combineDyn (<>) pureAttrs attrs

  textInput $ def
    & attributes .~ dynAttrs

purePasswordInput ph = textInput $ def
                   & attributes .~ ( constDyn $ "placeholder" =: ph)
                   & textInputConfig_inputType .~ ("password")

logSettingsForm :: MonadWidget t m => m (Dynamic t LogSettings, Event t ())
logSettingsForm = do
  d5 <- elAttr "form" ("class" =: "pure-form") $ do
    rec un <- mapDyn pack =<< value <$> pureTextInput "Username" (constDyn mempty)
        el "br" blank
        pw <- mapDyn pack =<< value <$> purePasswordInput "Password"
        el "br" blank
        nodes <- mapDyn (fmap pack . splitElem ',') =<< value <$> pureTextInput "Nodes" (constDyn mempty)
        el "br" blank
        lName <- mapDyn parseRelFile =<< value <$> pureTextInput "Logfile Name" textAttr
        el "br" blank
        hostUrl <- mapDyn pack =<< value <$> pureTextInput "Server URL" (constDyn mempty)
        el "br" blank

        textAttr <- mapDyn (checkField . validFileName) lName

        d1 <- combineDyn LogSettings un pw
        d2 <- combineDyn (\x y -> x y) d1 nodes
        d3 <- combineDyn (\x y -> x y) d2 lName
        d4 <- combineDyn (\x y -> x y) d3 (constDyn $ parseRelDir "logs/")
        d5 <- combineDyn (\x y -> x y) d4 hostUrl
    return d5

  click <- pureButton "Submit"
  return (d5, click)

validFileName :: Maybe (Path Rel File) -> Bool
validFileName Nothing = False
validFileName (Just _) = True

checkField :: Bool -> Map String String
checkField False = ("style" =: "border-color: red;")
checkField True = mempty

testXhr :: MonadWidget t m => Dynamic t LogSettings -> Event t () -> m ()
testXhr logSettings click = do
  let evt = fmap createRequest $ attachDyn logSettings (reqCfg <$ click)
      reqCfg = def { _xhrRequestConfig_headers = ("Content-type" =: "application/json")
                }
  dn <- count evt
  el "br" blank
  dyn =<< mapDyn (text . show :: MonadWidget t m => Int -> m ()) dn
  asyncReq <- performRequestAsync evt
  resp <- holdDyn Nothing $ fmap _xhrResponse_responseText asyncReq
  received <- mapDyn (fmap asSubmitStatus . join . fmap decodeText) resp
  el "br" blank
  dyn =<< mapDyn (mapM_ showSubmitStatus) received
  blank

showSubmitStatus :: MonadWidget t m => SubmitStatus -> m ()
showSubmitStatus Submitted = el "div" $ text "Downloading now"
showSubmitStatus (Confirmation files) = do
  elAttr "table" ("class" =: "pure-table") $ do
    el "thead" $ el "tr" $ el "th" $ text "filename"
    el "tbody" $ mapM (el "tr" . el "td" . createLink . fromRelFile) files
  blank
showSubmitStatus (SubmissionError msg) = el "div" $ text $ unpack msg

createLink :: MonadWidget t m => String -> m ()
createLink val = elAttr "a" ("href" =: val) $ text val

createRequest :: (LogSettings, XhrRequestConfig) -> XhrRequest
createRequest (info, cfg) = xhrRequest "POST" "http://localhost:8081/submit" cfg'
  where
    cfg' = cfg { _xhrRequestConfig_sendData = Just . unpack . decodeUtf8 . toStrict $ encode info }

doIt :: MonadWidget t m => m ()
doIt = do
  (logSettings, click) <- logSettingsForm
  -- dynText =<< mapDyn show logSettings
  testXhr logSettings click

handleMSettings :: MonadSample t m => Maybe LogSettings -> m LogSettings
handleMSettings Nothing = fail $ "Bad log file name"
handleMSettings (Just logSettings) = return logSettings

head :: MonadWidget t m => m ()
head = do
  elAttr "link" ( "rel" =: "stylesheet"
               <> "href" =: "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"
                ) blank

asSubmitStatus :: SubmitStatus -> SubmitStatus
asSubmitStatus = id
