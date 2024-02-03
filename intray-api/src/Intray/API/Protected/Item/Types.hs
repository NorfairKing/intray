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
    module Data.UUID.Typed,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID.Typed
import Import
import Intray.API.Types ()
import Intray.Data

data TypedItem = TypedItem
  { itemType :: ItemType,
    itemData :: ByteString
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TypedItem)

instance Validity TypedItem

instance HasCodec TypedItem where
  codec =
    object "TypedItem"
      $ TypedItem
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
  deriving (Show, Read, Eq, Ord, Generic)

data AddedItem a = AddedItem
  { addedItemContents :: a,
    addedItemCreated :: UTCTime
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (AddedItem a))

instance (Validity a) => Validity (AddedItem a)

instance (HasCodec a) => HasCodec (AddedItem a) where
  codec =
    object "AddedItem"
      $ AddedItem
      <$> requiredField "contents" "the item itself"
      .= addedItemContents
      <*> requiredField "created" "creation timestamp"
      .= addedItemCreated

data ItemInfo a = ItemInfo
  { itemInfoIdentifier :: ItemUUID,
    itemInfoContents :: a,
    itemInfoCreated :: UTCTime
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ItemInfo a))

instance (Validity a) => Validity (ItemInfo a)

instance (HasCodec a) => HasCodec (ItemInfo a) where
  codec =
    object "ItemInfo"
      $ ItemInfo
      <$> requiredField "id" "uuid"
      .= itemInfoIdentifier
      <*> requiredField "contents" "the item itself"
      .= itemInfoContents
      <*> requiredField "created" "creation timestamp"
      .= itemInfoCreated
