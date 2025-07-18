{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected.Item.Types
  ( ItemType (..),
    TypedItem (..),
    textTypedItem,
    TypedItemCase (..),
    typedItemCase,
    AddedItem (..),
    ItemInfo (..),
    ItemUUID,
    AlertEvent (..),
    Alert (..),
    module Data.UUID.Typed,
  )
where

import Autodocodec
import Control.Arrow (left)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID.Typed
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Intray.API.ItemType
import Intray.API.Types

data TypedItem = TypedItem
  { itemType :: ItemType,
    itemData :: ByteString
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TypedItem)

instance Validity TypedItem

instance HasCodec TypedItem where
  codec =
    object "TypedItem" $
      TypedItem
        <$> requiredField "type" "type of the item data"
          .= itemType
        <*> requiredFieldWith
          "data"
          ( bimapCodec
              (Base64.decode . SB8.pack)
              (SB8.unpack . Base64.encode)
              codec
          )
          "base64-encoded data"
          .= itemData

textTypedItem :: Text -> TypedItem
textTypedItem t = TypedItem {itemType = TextItem, itemData = TE.encodeUtf8 t}

typedItemCase :: TypedItem -> Either String TypedItemCase
typedItemCase TypedItem {..} =
  case itemType of
    TextItem -> left show $ CaseTextItem <$> TE.decodeUtf8' itemData
    ImageItem it -> pure $ CaseImageItem it itemData

data TypedItemCase
  = CaseTextItem Text
  | CaseImageItem ImageType ByteString

data AddedItem a = AddedItem
  { addedItemContents :: a,
    addedItemCreated :: UTCTime,
    addedItemAccessKeyName :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Generic)

instance (Validity a) => Validity (AddedItem a)

instance (HasCodec a) => HasCodec (AddedItem a) where
  codec =
    object "AddedItem" $
      AddedItem
        <$> requiredField "contents" "the item itself"
          .= addedItemContents
        <*> requiredField "created" "creation timestamp"
          .= addedItemCreated
        <*> optionalField "access-key" "access key used to create this item"
          .= addedItemAccessKeyName

data ItemInfo a = ItemInfo
  { itemInfoIdentifier :: ItemUUID,
    itemInfoContents :: a,
    itemInfoCreated :: UTCTime,
    itemInfoAccessKeyName :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ItemInfo a))

instance (Validity a) => Validity (ItemInfo a)

instance (HasCodec a) => HasCodec (ItemInfo a) where
  codec =
    object "ItemInfo" $
      ItemInfo
        <$> requiredField "id" "uuid"
          .= itemInfoIdentifier
        <*> requiredField "contents" "the item itself"
          .= itemInfoContents
        <*> requiredField "created" "creation timestamp"
          .= itemInfoCreated
        <*> optionalField "access-key" "access key used to create this item"
          .= itemInfoAccessKeyName

-- Docs:
-- https://prometheus.io/docs/alerting/latest/configuration/#webhook_config
--
-- Example from the docs:
--
-- {
--   "version": "4",
--   "groupKey": <string>,              // key identifying the group of alerts (e.g. to deduplicate)
--   "truncatedAlerts": <int>,          // how many alerts have been truncated due to "max_alerts"
--   "status": "<resolved|firing>",
--   "receiver": <string>,
--   "groupLabels": <object>,
--   "commonLabels": <object>,
--   "commonAnnotations": <object>,
--   "externalURL": <string>,           // backlink to the Alertmanager.
--   "alerts": [
--     {
--       "status": "<resolved|firing>",
--       "labels": <object>,
--       "annotations": <object>,
--       "startsAt": "<rfc3339>",
--       "endsAt": "<rfc3339>",
--       "generatorURL": <string>,      // identifies the entity that caused the alert
--       "fingerprint": <string>        // fingerprint to identify the alert
--     },
--     ...
--   ]
-- }
data AlertEvent = AlertEvent
  { alertEventVersion :: !Text,
    alertEventGroupKey :: !Text,
    alertEventTruncatedAlerts :: !(Maybe Int),
    alertEventStatus :: !Text,
    alertEventReceiver :: !Text,
    alertEventGroupLabels :: !(Map Text Text),
    alertEventCommonLabels :: !(Map Text Text),
    alertEventCommonAnnotations :: !(Map Text Text),
    alertEventExternalURL :: !Text,
    alertEventAlerts :: ![Alert]
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AlertEvent)

instance Validity AlertEvent

instance HasCodec AlertEvent where
  codec =
    object "AlertEvent" $
      AlertEvent
        <$> requiredField "version" "version of the alert event format"
          .= alertEventVersion
        <*> requiredField "groupKey" "key identifying the group of alerts"
          .= alertEventGroupKey
        <*> requiredField "truncatedAlerts" "how many alerts have been truncated due to 'max_alerts'"
          .= alertEventTruncatedAlerts
        <*> requiredField "status" "status of the alert event"
          .= alertEventStatus
        <*> requiredField "receiver" "receiver of the alert event"
          .= alertEventReceiver
        <*> optionalFieldWithOmittedDefault "groupLabels" M.empty "labels for the group"
          .= alertEventGroupLabels
        <*> optionalFieldWithOmittedDefault "commonLabels" M.empty "common labels for all alerts"
          .= alertEventCommonLabels
        <*> optionalFieldWithOmittedDefault "commonAnnotations" M.empty "common annotations for all alerts"
          .= alertEventCommonAnnotations
        <*> requiredField "externalURL" "backlink to the Alertmanager"
          .= alertEventExternalURL
        <*> optionalFieldWithOmittedDefault "alerts" [] "the list of alerts in this event"
          .= alertEventAlerts

data Alert = Alert
  { alertStatus :: !Text,
    alertLabels :: !(Map Text Text),
    alertAnnotations :: !(Map Text Text),
    alertStartsAt :: !UTCTime,
    alertEndsAt :: !UTCTime,
    alertGeneratorURL :: !Text,
    alertFingerprint :: !Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Alert

instance HasCodec Alert where
  codec =
    object "Alert" $
      Alert
        <$> requiredField "status" "status of the alert"
          .= alertStatus
        <*> optionalFieldWithOmittedDefault "labels" M.empty "labels for the alert"
          .= alertLabels
        <*> optionalFieldWithOmittedDefault "annotations" M.empty "annotations for the alert"
          .= alertAnnotations
        <*> requiredField "startsAt" "start time of the alert"
          .= alertStartsAt
        <*> requiredField "endsAt" "end time of the alert"
          .= alertEndsAt
        <*> requiredField "generatorURL" "URL that identifies the entity that caused the alert"
          .= alertGeneratorURL
        <*> requiredField "fingerprint" "fingerprint to identify the alert"
          .= alertFingerprint
