{-# LANGUAGE DataKinds #-}

module Intray.Client
  ( module Intray.Client,
    module Intray.API,
    module Servant.API,
    module Servant.Client,
    module Servant.Auth.Client,
    module Data.Mergeless,
    module Data.UUID.Typed,
  )
where

import Data.Aeson as JSON
import Data.Int
import Data.Mergeless
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import qualified Data.UUID.Typed
import Intray.API
import Servant.API hiding (Unique)
import Servant.API.Flatten
import Servant.Auth.Client
import Servant.Client

clientGetShowItem :: Token -> ClientM (Maybe (ItemInfo TypedItem))
clientGetSize :: Token -> ClientM Int
clientGetItemUUIDs :: Token -> ClientM [ItemUUID]
clientGetItems :: Token -> ClientM [ItemInfo TypedItem]
clientPostAddItem :: Token -> TypedItem -> ClientM ItemUUID
clientGetItem :: Token -> ItemUUID -> ClientM (ItemInfo TypedItem)
clientDeleteItem :: Token -> ItemUUID -> ClientM NoContent
clientPostSync ::
  Token ->
  SyncRequest Int64 ItemUUID (AddedItem TypedItem) ->
  ClientM (SyncResponse Int64 ItemUUID (AddedItem TypedItem))
clientPostAlert :: Token -> AlertEvent -> ClientM NoContent
clientGetAccountInfo :: Token -> ClientM AccountInfo
clientPostChangePassphrase :: Token -> ChangePassphrase -> ClientM NoContent
clientDeleteAccount :: Token -> ClientM NoContent
clientPostInitiateStripeCheckoutSession :: Token -> InitiateStripeCheckoutSession -> ClientM InitiatedCheckoutSession
clientPostAddAccessKey :: Token -> AddAccessKey -> ClientM AccessKeyCreated
clientGetAccessKey :: Token -> AccessKeyUUID -> ClientM AccessKeyInfo
clientGetAccessKeys :: Token -> ClientM [AccessKeyInfo]
clientDeleteAccessKey :: Token -> AccessKeyUUID -> ClientM NoContent
clientGetPermissions :: Token -> ClientM (Set Permission)
clientPostRegister :: Registration -> ClientM NoContent
clientPostLogin :: LoginForm -> ClientM (Headers '[Header "Set-Cookie" Text] NoContent)
clientGetPricing :: ClientM (Maybe Pricing)
clientPostStripeHook :: JSON.Value -> ClientM NoContent
clientAdminGetStats :: Token -> ClientM AdminStats
clientAdminDeleteAccount :: Token -> Username -> ClientM NoContent
clientAdminGetAccount :: Token -> Username -> ClientM AccountInfo
clientAdminGetAccounts :: Token -> ClientM [AccountInfo]
clientAdminPutAccountSubscription :: Token -> Username -> UTCTime -> ClientM NoContent
clientGetShowItem
  :<|> clientGetSize
  :<|> clientGetItemUUIDs
  :<|> clientGetItems
  :<|> clientPostAddItem
  :<|> clientGetItem
  :<|> clientDeleteItem
  :<|> clientPostSync
  :<|> clientPostAlert
  :<|> clientGetAccountInfo
  :<|> clientPostChangePassphrase
  :<|> clientDeleteAccount
  :<|> clientPostInitiateStripeCheckoutSession
  :<|> clientPostAddAccessKey
  :<|> clientGetAccessKey
  :<|> clientGetAccessKeys
  :<|> clientDeleteAccessKey
  :<|> clientGetPermissions
  :<|> clientPostRegister
  :<|> clientPostLogin
  :<|> clientGetPricing
  :<|> clientPostStripeHook
  :<|> clientAdminGetStats
  :<|> clientAdminDeleteAccount
  :<|> clientAdminGetAccount
  :<|> clientAdminGetAccounts
  :<|> clientAdminPutAccountSubscription =
    client (flatten intrayAPI)
