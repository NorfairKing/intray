{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Intray.API.Admin.Types where

import Data.Validity
import GHC.Generics (Generic)

data AdminStats = AdminStats
  { adminStatsNbAccounts :: !Word,
    adminStatsSubscribedUsers :: !Word,
    adminStatsNbItems :: !Word,
    adminStatsActiveUsers :: !ActiveUsers
  }
  deriving stock (Show, Generic)

instance Validity AdminStats

data ActiveUsers = ActiveUsers
  { activeUsersDaily :: !Word,
    activeUsersWeekly :: !Word,
    activeUsersMonthly :: !Word,
    activeUsersYearly :: !Word
  }
  deriving stock (Show, Generic)

instance Validity ActiveUsers
