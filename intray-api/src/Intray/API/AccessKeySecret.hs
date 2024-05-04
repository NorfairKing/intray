{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.API.AccessKeySecret
  ( AccessKeySecret,
    generateRandomAccessKeySecret,
    accessKeySecretText,
    parseAccessKeySecret,
    parseAccessKeySecretText,
  )
where

import Autodocodec
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Base16 as SB16
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Validity.ByteString ()
import Database.Persist.Sql
import GHC.Generics (Generic)
import System.Random

newtype AccessKeySecret
  = AccessKeySecret ByteString
  deriving stock (Show, Eq, Generic)
  deriving newtype (Validity, PersistField, PersistFieldSql)
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
