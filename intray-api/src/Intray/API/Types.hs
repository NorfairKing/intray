{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Types
  ( ProtectAPI,
    AccessKeyEnv (..),
    AuthCookie (..),
    Permission (..),
    userPermissions,
    adminPermissions,
    Registration (..),
    LoginForm (..),
    HashedPassword,
    passwordHash,
    validatePassword,
    ItemUUID,
    AccountUUID,
    AccessKeyUUID,
    AccessKeySecret,
    Username,
    parseUsername,
    parseUsernameWithError,
    usernameText,
    Pricing (..),
    module Data.UUID.Typed,
  )
where

import Autodocodec
import Control.Arrow (left)
import Control.Exception
import Control.Monad.Logger
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.UUID.Typed
import Data.Validity
import GHC.Generics (Generic)
import Intray.API.AccessKeySecret
import Intray.API.AccessKeyUUID
import Intray.API.AccountUUID
import Intray.API.HashedPassword
import Intray.API.ItemUUID
import Intray.API.Permission
import Intray.API.Username
import Network.Wai
import OptEnvConf
import Servant.Auth
import Servant.Auth.Server
import Servant.Auth.Server.Internal.Class

type ProtectAPI = Auth '[JWT, IntrayAccessKey] AuthCookie

data IntrayAccessKey

newtype AccessKeyEnv = AccessKeyEnv
  { accessKeyEnvAuthenticate :: Username -> Text -> IO (AuthResult AuthCookie)
  }

instance IsAuth IntrayAccessKey AuthCookie where
  type AuthArgs IntrayAccessKey = '[AccessKeyEnv]
  runAuth _ _ AccessKeyEnv {..} = AuthCheck $ \req ->
    case (,) <$> lookup "Username" (requestHeaders req) <*> lookup "Access-Key" (requestHeaders req) of
      Nothing -> pure Indefinite
      Just (unbs, akbs) ->
        case (,)
          <$> (left displayException (TE.decodeUtf8' unbs) >>= parseUsernameWithError)
          <*> left displayException (TE.decodeUtf8' akbs) of
          Left _ -> pure Indefinite
          Right (un, ak) -> accessKeyEnvAuthenticate un ak

data AuthCookie = AuthCookie
  { authCookieUserUUID :: AccountUUID,
    authCookiePermissions :: Set Permission,
    authCookieAccessKeyName :: Maybe Text
  }
  deriving (FromJSON, ToJSON) via (Autodocodec AuthCookie)

instance HasCodec AuthCookie where
  codec =
    object "AuthCookie" $
      AuthCookie
        <$> requiredField "uuid" "user uuid"
          .= authCookieUserUUID
        <*> optionalFieldWithOmittedDefault "permissions" S.empty "permissions"
          .= authCookiePermissions
        <*> optionalField "access-key" "access key name" .= authCookieAccessKeyName

instance FromJWT AuthCookie

instance ToJWT AuthCookie

data Registration = Registration
  { registrationUsername :: Username,
    registrationPassword :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Registration)

instance Validity Registration

instance HasCodec Registration where
  codec =
    object "Registration" $
      Registration
        <$> requiredField "name" "Username"
          .= registrationUsername
        <*> requiredField "password" "Password"
          .= registrationPassword

data LoginForm = LoginForm
  { loginFormUsername :: Username,
    loginFormPassword :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec LoginForm)

instance Validity LoginForm

instance HasCodec LoginForm where
  codec =
    object "LoginForm" $
      LoginForm
        <$> requiredField "username" "Username"
          .= loginFormUsername
        <*> requiredField "password" "Password"
          .= loginFormPassword

data Pricing = Pricing
  { pricingPlan :: !Text,
    pricingPrice :: !Text,
    pricingStripePublishableKey :: !Text,
    pricingMaxItemsFree :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Pricing)

instance Validity Pricing

instance HasCodec Pricing where
  codec =
    object "Pricing" $
      Pricing
        <$> requiredField "plan" "stripe plan"
          .= pricingPlan
        <*> requiredField "price" "price"
          .= pricingPrice
        <*> requiredField "publishable-key" "publishable key"
          .= pricingStripePublishableKey
        <*> requiredField "max-items-free" "how many items a free account can have"
          .= pricingMaxItemsFree

instance HasCodec LogLevel where
  codec =
    stringConstCodec
      [ (LevelDebug, "Debug"),
        (LevelInfo, "Info"),
        (LevelWarn, "Warn"),
        (LevelError, "Error")
      ]

instance HasParser LogLevel where
  settingsParser =
    setting
      [ help "minimal severity for log message",
        reader $
          maybeReader $
            \case
              "Debug" -> Just LevelDebug
              "Info" -> Just LevelInfo
              "Warn" -> Just LevelWarn
              "Error" -> Just LevelError
              _ -> Nothing,
        metavar "LOG_LEVEL",
        name "log-level",
        value LevelInfo
      ]
