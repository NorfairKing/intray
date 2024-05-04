{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.API.Username
  ( Username (),
    parseUsername,
    parseUsernameWithError,
    usernameText,
    validUsernameChar,
  )
where

import Autodocodec
import Control.Arrow (left)
import Data.Aeson as JSON
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist.Sql
import GHC.Generics (Generic)
import Web.HttpApiData
import Web.PathPieces

newtype Username = Username
  { usernameText :: Text
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Username)

instance Validity Username where
  validate (Username t) =
    mconcat
      [ check (not (T.null t)) "The username is not empty.",
        check (T.length t >= 3) "The username is at least three characters long.",
        mconcat $
          flip map (zip [1 ..] $ map UsernameChar $ T.unpack t) $
            \(ix, uc@(UsernameChar c)) ->
              annotate uc $ unwords ["character number", show (ix :: Int), "of the username:", show c]
      ]

instance PersistField Username where
  toPersistValue (Username t) = PersistText t
  fromPersistValue pv = do
    t <- fromPersistValue pv
    left T.pack $ parseUsernameWithError t

instance PersistFieldSql Username where
  sqlType _ = SqlString

instance HasCodec Username where
  codec = bimapCodec parseUsernameWithError usernameText codec

instance PathPiece Username where
  fromPathPiece = parseUsername
  toPathPiece = usernameText

instance ToHttpApiData Username where
  toUrlPiece = usernameText
  toQueryParam = usernameText

instance FromHttpApiData Username where
  parseUrlPiece = left T.pack . parseUsernameWithError
  parseQueryParam = left T.pack . parseUsernameWithError

parseUsername :: (MonadFail m) => Text -> m Username
parseUsername t =
  case parseUsernameWithError t of
    Left err -> fail err
    Right un -> pure un

parseUsernameWithError :: Text -> Either String Username
parseUsernameWithError = prettyValidate . Username

newtype UsernameChar
  = UsernameChar Char

instance Validity UsernameChar where
  validate (UsernameChar '-') = valid
  validate (UsernameChar '_') = valid
  validate (UsernameChar c) =
    mconcat
      [ check (not (Char.isControl c)) "The character is not a control character.",
        check (Char.isAlphaNum c) "The character is alphanumeric.",
        check (Char.isLatin1 c) "The character is part of Latin1."
      ]

validUsernameChar :: Char -> Bool
validUsernameChar = isValid . UsernameChar
