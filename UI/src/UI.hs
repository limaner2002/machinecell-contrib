{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI where

import Types
import ClassyPrelude
import Reflex
import Reflex.Dom
import Data.Default
import Path

logSettingsForm :: MonadWidget t m => m (Dynamic t (Maybe LogSettings))
logSettingsForm = do
  un <- mapDyn pack =<< value <$> textInput def
  el "br" blank
  pw <- mapDyn pack =<< value <$> textInput def
  el "br" blank
  nodes <- mapDyn (fmap pack . splitElem ',') =<< value <$> textInput def
  el "br" blank
  lName <- mapDyn parseRelFile =<< value <$> textInput def
  el "br" blank
  hostUrl <- mapDyn pack =<< value <$> textInput def
  d1 <- combineDyn LogSettings un pw
  d2 <- combineDyn (\x y -> x y) d1 nodes
  d3 <- combineDyn (\x y -> x <$> y) d2 lName
  d4 <- combineDyn (\x y -> x <*> y) d3 (constDyn $ parseRelDir "logs/")
  d5 <- combineDyn (\x y -> x <*> pure y) d4 hostUrl
  return d5
