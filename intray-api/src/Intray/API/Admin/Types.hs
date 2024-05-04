{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.API.Admin.Types where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Validity
import GHC.Generics (Generic)
import Intray.API.Types ()

data AdminStats = AdminStats
  { adminStatsNbAccounts :: !Word,
    adminStatsSubscribedUsers :: !Word,
    adminStatsNbItems :: !Word,
    adminStatsActiveUsers :: !ActiveUsers
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AdminStats)

instance Validity AdminStats

instance HasCodec AdminStats where
  codec =
    object "AdminStats" $
      AdminStats
        <$> requiredField "accounts" "how many accounts there are"
          .= adminStatsNbAccounts
        <*> requiredField "subscribed-user" "how many of those are subscribed"
          .= adminStatsSubscribedUsers
        <*> requiredField "items" "total number of items that users have in their intrays"
          .= adminStatsNbItems
        <*> requiredField "active-users" "X-active users stats"
          .= adminStatsActiveUsers

data ActiveUsers = ActiveUsers
  { activeUsersDaily :: !Word,
    activeUsersWeekly :: !Word,
    activeUsersMonthly :: !Word,
    activeUsersYearly :: !Word
  }
  deriving stock (Show, Eq, Generic)

instance Validity ActiveUsers

instance HasCodec ActiveUsers where
  codec =
    object "ActiveUsers" $
      ActiveUsers
        <$> requiredField "daily" "daily active users"
          .= activeUsersDaily
        <*> requiredField "weekly" "weekly active users"
          .= activeUsersWeekly
        <*> requiredField "monthly" "monthly active users"
          .= activeUsersMonthly
        <*> requiredField "yearly" "yearly active users"
          .= activeUsersYearly
