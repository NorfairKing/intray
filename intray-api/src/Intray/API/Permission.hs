{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.API.Permission where

import Autodocodec
import Data.Aeson
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Text.Read

data Permission
  = PermitAdd
  | PermitShow
  | PermitSize
  | PermitDelete
  | PermitGetItem
  | PermitGetItems
  | PermitGetItemUUIDs
  | PermitSync
  | PermitAlert
  | PermitDeleteAccount
  | PermitGetAccountInfo
  | PermitPostChangePassphrase
  | PermitPostAddAccessKey
  | PermitGetAccessKey
  | PermitGetAccessKeys
  | PermitDeleteAccessKey
  | PermitGetPermissions
  | PermitInitiateCheckout
  | PermitAdminDeleteAccount
  | PermitAdminGetAccounts
  | PermitAdminGetAccount
  | PermitAdminGetStats
  | PermitAdminPutAccountSubscription
  deriving stock (Show, Read, Eq, Ord, Generic, Enum, Bounded)
  deriving (FromJSON, ToJSON) via (Autodocodec Permission)

instance Validity Permission

instance HasCodec Permission where
  codec = shownBoundedEnumCodec

instance PersistField Permission where
  toPersistValue = PersistText . T.pack . show
  fromPersistValue pv = do
    t <- fromPersistValueText pv
    case readMaybe $ T.unpack t of
      Nothing -> Left "Unknown Permission value."
      Just p -> pure p

instance PersistFieldSql Permission where
  sqlType Proxy = SqlString

userPermissions :: Set Permission
userPermissions =
  S.fromList
    [ PermitAdd,
      PermitShow,
      PermitSize,
      PermitDelete,
      PermitGetItem,
      PermitGetItems,
      PermitGetItemUUIDs,
      PermitSync,
      PermitAlert,
      PermitDeleteAccount,
      PermitGetAccountInfo,
      PermitPostChangePassphrase,
      PermitPostAddAccessKey,
      PermitGetAccessKey,
      PermitGetAccessKeys,
      PermitDeleteAccessKey,
      PermitGetPermissions,
      PermitInitiateCheckout
    ]

adminPermissions :: Set Permission
adminPermissions = S.fromList [minBound .. maxBound] -- All
