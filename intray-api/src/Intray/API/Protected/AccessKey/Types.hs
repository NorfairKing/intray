{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.API.Protected.AccessKey.Types
  ( AccessKeyInfo (..),
    AccessKeyUUID,
    AddAccessKey (..),
    AccessKeyCreated (..),
    module Data.UUID.Typed,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.UUID.Typed
import Intray.API.AccessKeySecret
import Intray.API.Types

data AccessKeyInfo = AccessKeyInfo
  { accessKeyInfoUUID :: AccessKeyUUID,
    accessKeyInfoName :: Text,
    accessKeyInfoCreatedTimestamp :: UTCTime,
    accessKeyInfoPermissions :: Set Permission
  }
  deriving (FromJSON, ToJSON) via (Autodocodec AccessKeyInfo)

instance HasCodec AccessKeyInfo where
  codec =
    object "AccessKeyInfo" $
      AccessKeyInfo
        <$> requiredField "uuid" "access key uuid"
          .= accessKeyInfoUUID
        <*> requiredField "name" "access key name"
          .= accessKeyInfoName
        <*> requiredField "created" "creation time"
          .= accessKeyInfoCreatedTimestamp
        <*> requiredField "permissions" "permissions"
          .= accessKeyInfoPermissions

data AddAccessKey = AddAccessKey
  { addAccessKeyName :: Text,
    addAccessKeyPermissions :: Set Permission
  }
  deriving (FromJSON, ToJSON) via (Autodocodec AddAccessKey)

instance HasCodec AddAccessKey where
  codec =
    object "AddAccessKey" $
      AddAccessKey
        <$> requiredField "name" "access key name"
          .= addAccessKeyName
        <*> requiredField "permissions" "access key permissions"
          .= addAccessKeyPermissions

data AccessKeyCreated = AccessKeyCreated
  { accessKeyCreatedCreatedTimestamp :: UTCTime,
    accessKeyCreatedKey :: AccessKeySecret,
    accessKeyCreatedUUID :: AccessKeyUUID
  }
  deriving (FromJSON, ToJSON) via (Autodocodec AccessKeyCreated)

instance HasCodec AccessKeyCreated where
  codec =
    object "AccessKeyCreated" $
      AccessKeyCreated
        <$> requiredField "created" "created timestamp"
          .= accessKeyCreatedCreatedTimestamp
        <*> requiredField "secret" "access key secret"
          .= accessKeyCreatedKey
        <*> requiredField "uuid" "access key uuid"
          .= accessKeyCreatedUUID
