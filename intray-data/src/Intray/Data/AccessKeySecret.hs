{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Data.AccessKeySecret
  ( AccessKeySecret,
    generateRandomAccessKeySecret,
    accessKeySecretText,
    parseAccessKeySecret,
    parseAccessKeySecretText,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Base16 as SB16
import qualified Data.Text.Encoding as TE
import Database.Persist
import Database.Persist.Sql
import Intray.Data.Import
import System.Random

newtype AccessKeySecret
  = AccessKeySecret ByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (PersistField, PersistFieldSql, Validity)
  deriving (FromJSON, ToJSON) via (Autodocodec AccessKeySecret)

instance HasCodec AccessKeySecret where
  codec = bimapCodec parseAccessKeySecretText accessKeySecretText codec

accessKeySecretText :: AccessKeySecret -> Text
accessKeySecretText (AccessKeySecret bs) = TE.decodeUtf8 $ SB16.encode bs

parseAccessKeySecret :: Text -> Maybe AccessKeySecret
parseAccessKeySecret t =
  case parseAccessKeySecretText t of
    Left _ -> Nothing
    Right acs -> Just acs

parseAccessKeySecretText :: Text -> Either String AccessKeySecret
parseAccessKeySecretText t =
  case SB16.decode $ TE.encodeUtf8 t of
    Right d -> Right $ AccessKeySecret d
    Left err -> Left $ unlines [unwords ["Invalid Base16 access key secret: " <> show t, err]]

generateRandomAccessKeySecret :: IO AccessKeySecret
generateRandomAccessKeySecret = AccessKeySecret . SB.pack <$> replicateM 16 randomIO
