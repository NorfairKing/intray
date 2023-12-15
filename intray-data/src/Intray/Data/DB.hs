{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

module Intray.Data.DB where

import Data.Set (Set)
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH
import Intray.Data.AccessKeyUUID
import Intray.Data.AccountUUID
import Intray.Data.HashedPassword
import Intray.Data.Import
import Intray.Data.ItemType
import Intray.Data.ItemUUID
import Intray.Data.Permission
import Intray.Data.Username

share
  [mkPersist sqlSettings, mkMigrate "serverAutoMigration"]
  [persistLowerCase|

User
    identifier AccountUUID
    username Username
    hashedPassword HashedPassword
    createdTimestamp UTCTime
    lastLogin UTCTime Maybe

    UniqueUserIdentifier identifier
    UniqueUsername username

    deriving Show
    deriving Eq
    deriving Generic


StripeCustomer sql=customer
    user AccountUUID
    customer Text sql=stripe_customer
    UniqueStripeCustomer user customer

    deriving Show
    deriving Eq
    deriving Generic

Subscription
    user AccountUUID
    end UTCTime

    UniqueSubscriptionUser user

    deriving Show
    deriving Eq
    deriving Generic


IntrayItem
    identifier ItemUUID
    userId AccountUUID
    type ItemType
    contents ByteString
    created UTCTime

    UniqueItemIdentifier identifier userId

    deriving Show
    deriving Eq
    deriving Generic


AccessKey
    identifier AccessKeyUUID
    user AccountUUID
    name Text
    hashedKey HashedPassword
    createdTimestamp UTCTime
    permissions (Set Permission)

    UniqueAccessKeyIdentifier identifier user

    deriving Show
    deriving Eq
    deriving Generic
|]

instance Validity IntrayItem

instance Validity User

instance Validity AccessKey
