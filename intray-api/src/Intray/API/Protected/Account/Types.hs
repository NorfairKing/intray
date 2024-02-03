{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.API.Protected.Account.Types
  ( module Intray.API.Protected.Account.Types,
    module Data.UUID.Typed,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Time
import Data.UUID.Typed
import Import
import Intray.API.Types ()
import Intray.Data

data AccountInfo = AccountInfo
  { accountInfoUUID :: AccountUUID,
    accountInfoUsername :: Username,
    accountInfoCreatedTimestamp :: UTCTime,
    accountInfoLastLogin :: Maybe UTCTime,
    accountInfoAdmin :: Bool,
    accountInfoCount :: Int,
    accountInfoStatus :: PaidStatus
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AccountInfo)

instance Validity AccountInfo

instance HasCodec AccountInfo where
  codec =
    object "AccountInfo"
      $ AccountInfo
      <$> requiredField "uuid" "account uuid"
      .= accountInfoUUID
      <*> requiredField "username" "account username"
      .= accountInfoUsername
      <*> requiredField "created" "creation time"
      .= accountInfoCreatedTimestamp
      <*> requiredField "last-login" "last login time"
      .= accountInfoLastLogin
      <*> requiredField "admin" "whether the user is an admin"
      .= accountInfoAdmin
      <*> requiredField "count" "how many items the user has in their intray"
      .= accountInfoCount
      <*> requiredField "status" "paid status of the account"
      .= accountInfoStatus

data PaidStatus
  = HasNotPaid Int -- Number of extra items that they're still allowed
  | HasPaid UTCTime
  | NoPaymentNecessary
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec PaidStatus)

instance Validity PaidStatus

instance HasCodec PaidStatus where
  codec =
    object "PaidStatus"
      $ dimapCodec f g
      $ eitherCodec (requiredField "items-left" "how many free items the user has left")
      $ eitherCodec
        (requiredField "until" "when the subscription expires")
        (pure NoPaymentNecessary)
    where
      f = \case
        Left i -> HasNotPaid i
        Right (Left t) -> HasPaid t
        Right (Right ps) -> ps
      g = \case
        HasNotPaid i -> Left i
        HasPaid t -> Right (Left t)
        NoPaymentNecessary -> Right (Right NoPaymentNecessary)

data ChangePassphrase = ChangePassphrase
  { changePassphraseOld :: Text,
    changePassphraseNew :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ChangePassphrase)

instance Validity ChangePassphrase

instance HasCodec ChangePassphrase where
  codec =
    object "ChangePassphrase"
      $ ChangePassphrase
      <$> requiredField "old-passphrase" "old passphrase"
      .= changePassphraseOld
      <*> requiredField "new-passphrase" "new passphrase"
      .= changePassphraseNew

data InitiateStripeCheckoutSession = InitiateStripeCheckoutSession
  { initiateStripeCheckoutSessionSuccessUrl :: Text,
    initiateStripeCheckoutSessionCanceledUrl :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec InitiateStripeCheckoutSession)

instance Validity InitiateStripeCheckoutSession

instance HasCodec InitiateStripeCheckoutSession where
  codec =
    object "InitiateStripeCheckoutSession"
      $ InitiateStripeCheckoutSession
      <$> requiredField "success" "success url"
      .= initiateStripeCheckoutSessionSuccessUrl
      <*> requiredField "canceled" "canceled url"
      .= initiateStripeCheckoutSessionCanceledUrl

data InitiatedCheckoutSession = InitiatedCheckoutSession
  { initiatedCheckoutSessionId :: Text,
    initiatedCheckoutSessionCustomerId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec InitiatedCheckoutSession)

instance Validity InitiatedCheckoutSession

instance HasCodec InitiatedCheckoutSession where
  codec =
    object "InitiatedCheckoutSession"
      $ InitiatedCheckoutSession
      <$> requiredField "session" "session identifier"
      .= initiatedCheckoutSessionId
      <*> requiredField "customer" "customer identifier"
      .= initiatedCheckoutSessionCustomerId
