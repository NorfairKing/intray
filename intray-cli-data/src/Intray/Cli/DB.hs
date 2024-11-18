{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-name-shadowing #-}

module Intray.Cli.DB where

import Autodocodec
import Data.Aeson (FromJSONKey (..), ToJSONKey (..))
import Data.ByteString (ByteString)
import Data.Functor.Contravariant
import Data.Text (Text)
import Data.Time
import Database.Persist.Sql as Sql
import Database.Persist.TH
import Intray.API

share
  [mkPersist sqlSettings, mkMigrate "clientAutoMigration"]
  [persistLowerCase|

ClientItem
    type ItemType
    contents ByteString
    created UTCTime
    accessKeyName Text Maybe default=NULL

    serverIdentifier ItemUUID Maybe
    deleted Bool

    deriving Show
    deriving Eq


ShownItem
    item ClientItemId

    UniqueShownItem item -- This means there can only be 0 or 1

    deriving Show
    deriving Eq

|]

instance (ToBackendKey SqlBackend a) => HasCodec (Sql.Key a) where
  codec = dimapCodec toSqlKey fromSqlKey codec

instance (ToBackendKey SqlBackend a) => ToJSONKey (Sql.Key a) where
  toJSONKey = contramap fromSqlKey toJSONKey

instance (ToBackendKey SqlBackend a) => FromJSONKey (Sql.Key a) where
  fromJSONKey = toSqlKey <$> fromJSONKey
