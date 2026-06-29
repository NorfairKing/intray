{-# LANGUAGE DataKinds #-}

module Intray.API.Protected.AccessKey.Types
  ( AccessKeyInfo (..),
    AccessKeyUUID,
    AddAccessKey (..),
    AccessKeyCreated (..),
    module Data.UUID.Typed,
  )
where

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

data AddAccessKey = AddAccessKey
  { addAccessKeyName :: Text,
    addAccessKeyPermissions :: Set Permission
  }

data AccessKeyCreated = AccessKeyCreated
  { accessKeyCreatedCreatedTimestamp :: UTCTime,
    accessKeyCreatedKey :: AccessKeySecret,
    accessKeyCreatedUUID :: AccessKeyUUID
  }
