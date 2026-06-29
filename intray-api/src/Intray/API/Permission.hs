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
  | PermitDelete
  | PermitGetItem
  | PermitSync
  | PermitDeleteAccount
  | PermitGetAccountInfo
  | PermitPostChangePassphrase
  | PermitPostAddAccessKey
  | PermitGetAccessKeys
  | PermitDeleteAccessKey
  | PermitInitiateCheckout
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
  PermitDelete -> "PermitDelete"
  PermitGetItem -> "PermitGetItem"
  PermitSync -> "PermitSync"
  PermitDeleteAccount -> "PermitDeleteAccount"
  PermitGetAccountInfo -> "PermitGetAccountInfo"
  PermitPostChangePassphrase -> "PermitPostChangePassphrase"
  PermitPostAddAccessKey -> "PermitPostAddAccessKey"
  PermitGetAccessKeys -> "PermitGetAccessKeys"
  PermitDeleteAccessKey -> "PermitDeleteAccessKey"
  PermitInitiateCheckout -> "PermitInitiateCheckout"
  PermitOther t -> t

-- This needs to be permissive to allow for deleting permissions without
-- breaking existing clients.
parsePermission :: Text -> Permission
parsePermission = \case
  "PermitAdd" -> PermitAdd
  "PermitShow" -> PermitShow
  "PermitDelete" -> PermitDelete
  "PermitGetItem" -> PermitGetItem
  "PermitSync" -> PermitSync
  "PermitDeleteAccount" -> PermitDeleteAccount
  "PermitGetAccountInfo" -> PermitGetAccountInfo
  "PermitPostChangePassphrase" -> PermitPostChangePassphrase
  "PermitPostAddAccessKey" -> PermitPostAddAccessKey
  "PermitGetAccessKeys" -> PermitGetAccessKeys
  "PermitDeleteAccessKey" -> PermitDeleteAccessKey
  "PermitInitiateCheckout" -> PermitInitiateCheckout
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
      PermitDelete,
      PermitGetItem,
      PermitSync,
      PermitDeleteAccount,
      PermitGetAccountInfo,
      PermitPostChangePassphrase,
      PermitPostAddAccessKey,
      PermitGetAccessKeys,
      PermitDeleteAccessKey,
      PermitInitiateCheckout
    ]
