{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.API.Permission where

import Autodocodec
import Data.Aeson
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)

data Permission
  = PermitAdd
  | PermitShow
  | PermitSize
  | PermitDelete
  | PermitGetItem
  | PermitGetItems
  | PermitGetItemUUIDs
  | PermitSync
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
  | PermitOther Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Permission)

instance Validity Permission

instance HasCodec Permission where
  codec = dimapCodec parsePermission renderPermission codec

renderPermission :: Permission -> Text
renderPermission = \case
  PermitAdd -> "PermitAdd"
  PermitShow -> "PermitShow"
  PermitSize -> "PermitSize"
  PermitDelete -> "PermitDelete"
  PermitGetItem -> "PermitGetItem"
  PermitGetItems -> "PermitGetItems"
  PermitGetItemUUIDs -> "PermitGetItemUUIDs"
  PermitSync -> "PermitSync"
  PermitDeleteAccount -> "PermitDeleteAccount"
  PermitGetAccountInfo -> "PermitGetAccountInfo"
  PermitPostChangePassphrase -> "PermitPostChangePassphrase"
  PermitPostAddAccessKey -> "PermitPostAddAccessKey"
  PermitGetAccessKey -> "PermitGetAccessKey"
  PermitGetAccessKeys -> "PermitGetAccessKeys"
  PermitDeleteAccessKey -> "PermitDeleteAccessKey"
  PermitGetPermissions -> "PermitGetPermissions"
  PermitInitiateCheckout -> "PermitInitiateCheckout"
  PermitAdminDeleteAccount -> "PermitAdminDeleteAccount"
  PermitAdminGetAccounts -> "PermitAdminGetAccounts"
  PermitAdminGetAccount -> "PermitAdminGetAccount"
  PermitAdminGetStats -> "PermitAdminGetStats"
  PermitAdminPutAccountSubscription -> "PermitAdminPutAccountSubscription"
  PermitOther t -> t

-- This needs to be permissive to allow for deleting permissions without
-- breaking existing clients.
parsePermission :: Text -> Permission
parsePermission = \case
  "PermitAdd" -> PermitAdd
  "PermitShow" -> PermitShow
  "PermitSize" -> PermitSize
  "PermitDelete" -> PermitDelete
  "PermitGetItem" -> PermitGetItem
  "PermitGetItems" -> PermitGetItems
  "PermitGetItemUUIDs" -> PermitGetItemUUIDs
  "PermitSync" -> PermitSync
  "PermitDeleteAccount" -> PermitDeleteAccount
  "PermitGetAccountInfo" -> PermitGetAccountInfo
  "PermitPostChangePassphrase" -> PermitPostChangePassphrase
  "PermitPostAddAccessKey" -> PermitPostAddAccessKey
  "PermitGetAccessKey" -> PermitGetAccessKey
  "PermitGetAccessKeys" -> PermitGetAccessKeys
  "PermitDeleteAccessKey" -> PermitDeleteAccessKey
  "PermitGetPermissions" -> PermitGetPermissions
  "PermitInitiateCheckout" -> PermitInitiateCheckout
  "PermitAdminDeleteAccount" -> PermitAdminDeleteAccount
  "PermitAdminGetAccounts" -> PermitAdminGetAccounts
  "PermitAdminGetAccount" -> PermitAdminGetAccount
  "PermitAdminGetStats" -> PermitAdminGetStats
  "PermitAdminPutAccountSubscription" -> PermitAdminPutAccountSubscription
  t -> PermitOther t

instance PersistField Permission where
  toPersistValue = PersistText . T.pack . show
  fromPersistValue pv = do
    t <- fromPersistValueText pv
    Right $ parsePermission t

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
adminPermissions =
  -- All permissions
  S.fromList
    [ PermitAdd,
      PermitShow,
      PermitSize,
      PermitDelete,
      PermitGetItem,
      PermitGetItems,
      PermitGetItemUUIDs,
      PermitSync,
      PermitDeleteAccount,
      PermitGetAccountInfo,
      PermitPostChangePassphrase,
      PermitPostAddAccessKey,
      PermitGetAccessKey,
      PermitGetAccessKeys,
      PermitDeleteAccessKey,
      PermitGetPermissions,
      PermitInitiateCheckout,
      PermitAdminDeleteAccount,
      PermitAdminGetAccounts,
      PermitAdminGetAccount,
      PermitAdminGetStats,
      PermitAdminPutAccountSubscription
    ]
