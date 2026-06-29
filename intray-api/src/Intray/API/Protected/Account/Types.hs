{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Intray.API.Protected.Account.Types
  ( module Intray.API.Protected.Account.Types,
    module Data.UUID.Typed,
  )
where

import Data.Text (Text)
import Data.Time
import Data.UUID.Typed
import Data.Validity
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Intray.API.Types

data AccountInfo = AccountInfo
  { accountInfoUUID :: AccountUUID,
    accountInfoUsername :: Username,
    accountInfoCreatedTimestamp :: UTCTime,
    accountInfoLastLogin :: Maybe UTCTime,
    accountInfoAdmin :: Bool,
    accountInfoCount :: Int,
    accountInfoStatus :: PaidStatus
  }
  deriving stock (Show, Generic)

instance Validity AccountInfo

data PaidStatus
  = HasNotPaid Int -- Number of extra items that they're still allowed
  | HasPaid UTCTime
  | NoPaymentNecessary
  deriving stock (Show, Generic)

instance Validity PaidStatus

data ChangePassphrase = ChangePassphrase
  { changePassphraseOld :: Text,
    changePassphraseNew :: Text
  }
  deriving stock (Show, Generic)

instance Validity ChangePassphrase
