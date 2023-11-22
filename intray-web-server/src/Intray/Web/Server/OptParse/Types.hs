{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.OptParse.Types where

import Autodocodec
import Control.Arrow
import Control.Monad.Logger
import Data.Aeson (FromJSON, ToJSON)
import Import
import Intray.Client

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagPort :: !(Maybe Int),
    flagAPIBaseUrl :: !(Maybe BaseUrl),
    flagLogLevel :: !(Maybe LogLevel),
    flagTracking :: !(Maybe Text),
    flagVerification :: !(Maybe Text)
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confPort :: !(Maybe Int),
    confAPIBaseUrl :: !(Maybe BaseUrl),
    confLogLevel :: !(Maybe LogLevel),
    confTracking :: !(Maybe Text),
    confVerification :: !(Maybe Text)
  }
  deriving stock (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldOrNull "port" "The port to serve web requests on"
          .= confPort
        <*> optionalFieldOrNullWith "api-url" (bimapCodec (left show . parseBaseUrl) show codec) "The url to contact the api server at"
          .= confAPIBaseUrl
        <*> optionalFieldOrNull "log-level" "The minimal severity of log messages"
          .= confLogLevel
        <*> optionalFieldOrNull "tracking" "The google analytics tracking code"
          .= confTracking
        <*> optionalFieldOrNull "verification" "The google search console verification code"
          .= confVerification

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envPort :: !(Maybe Int),
    envLogLevel :: !(Maybe LogLevel),
    envAPIBaseUrl :: !(Maybe BaseUrl),
    envTracking :: !(Maybe Text),
    envVerification :: !(Maybe Text)
  }
  deriving (Show, Eq)

data Settings = Settings
  { setPort :: !Int,
    setLogLevel :: !LogLevel,
    setAPIBaseUrl :: !BaseUrl,
    setTracking :: !(Maybe Text),
    setVerification :: !(Maybe Text)
  }
  deriving (Show)
