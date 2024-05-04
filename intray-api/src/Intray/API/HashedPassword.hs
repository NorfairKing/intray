{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.API.HashedPassword
  ( passwordHash,
    HashedPassword (),
    validatePassword,
  )
where

import qualified Crypto.BCrypt as BCrypt
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Validity
import Database.Persist.Sql

newtype HashedPassword
  = HashedPassword ByteString
  deriving (Show, Eq, PersistField, PersistFieldSql)

instance Validity HashedPassword where
  validate (HashedPassword password) =
    declare "The password uses our chosen hashing policy" $
      BCrypt.hashUsesPolicy hashingpolicy password

hashingpolicy :: BCrypt.HashingPolicy
hashingpolicy = BCrypt.fastBcryptHashingPolicy

passwordHash :: Text -> IO (Maybe HashedPassword)
passwordHash =
  fmap (fmap HashedPassword) . BCrypt.hashPasswordUsingPolicy hashingpolicy . TE.encodeUtf8

validatePassword :: HashedPassword -> Text -> Bool
validatePassword (HashedPassword hp) = BCrypt.validatePassword hp . TE.encodeUtf8
