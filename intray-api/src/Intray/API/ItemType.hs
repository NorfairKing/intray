{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.API.ItemType where

import Autodocodec
import Control.Arrow
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)

data ItemType
  = TextItem
  | ImageItem ImageType
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ItemType)

instance Validity ItemType

instance PersistField ItemType where
  toPersistValue = toPersistValue . renderItemType
  fromPersistValue =
    left T.pack . \case
      PersistByteString bs -> do
        t <- left show $ TE.decodeUtf8' bs
        parseItemType t
      PersistText t -> parseItemType t
      _ -> Left "Not a valid ItemType"

instance PersistFieldSql ItemType where
  sqlType Proxy = SqlString

instance HasCodec ItemType where
  codec = bimapCodec parseItemType renderItemType codec

renderItemType :: ItemType -> Text
renderItemType =
  \case
    TextItem -> "text"
    ImageItem it -> renderImageType it

parseItemType :: Text -> Either String ItemType
parseItemType =
  \case
    "text" -> pure TextItem
    t -> case parseImageType t of
      Left _ -> Left $ "Unknown item type: " <> show t
      Right it -> pure $ ImageItem it

data ImageType
  = JpgImage
  | PngImage
  deriving (Show, Eq, Ord, Generic)

instance Validity ImageType

parseImageType :: Text -> Either String ImageType
parseImageType =
  \case
    "image/jpeg" -> Right JpgImage
    "image/png" -> Right PngImage
    t -> Left $ "Unknown image type: " <> show t

renderImageType :: ImageType -> Text
renderImageType =
  \case
    JpgImage -> "image/jpeg"
    PngImage -> "image/png"
