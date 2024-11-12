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
import Control.Arrow (left)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
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
