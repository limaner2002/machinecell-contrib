{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import ClassyPrelude
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Path

data LogSettings = LogSettings
  { username :: Text
  , password :: Text
  , nodeNames :: [Text]
  , logName :: Maybe (Path Rel File)
  , logDestination :: Maybe (Path Rel Dir)
  , logUrl :: Text
  } deriving (Show, Generic)

instance ToJSON LogSettings
instance FromJSON LogSettings

data SubmitStatus
  = Confirmation [Text]
  | Submitted
  | SubmissionError Text
  deriving (Show, Generic)

instance ToJSON SubmitStatus
instance FromJSON SubmitStatus

