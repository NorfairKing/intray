{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Data.UUID
  (
  )
where

import qualified Data.ByteString.Lazy as LB
import qualified Data.UUID as UUID
import Data.UUID.Typed
import Database.Persist
import Database.Persist.Sql
import Intray.Data.Import

instance PersistField (UUID a) where
  toPersistValue (UUID uuid) = PersistByteString $ LB.toStrict $ UUID.toByteString uuid
  fromPersistValue pv = do
    bs <- fromPersistValue pv
    case UUID.fromByteString $ LB.fromStrict bs of
      Nothing -> Left "Invalidy Bytestring to convert to UUID"
      Just uuid -> Right $ UUID uuid

instance PersistFieldSql (UUID a) where
  sqlType Proxy = SqlBlob
