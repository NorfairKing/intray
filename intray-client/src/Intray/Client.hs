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
import Data.Text (Text)
import qualified Data.UUID.Typed
import Intray.API
import Servant.API hiding (Unique)
import Servant.API.Flatten
import Servant.Auth.Client
import Servant.Client

clientPostRegister :: Registration -> ClientM NoContent
clientPostLogin :: LoginForm -> ClientM (Headers '[Header "Set-Cookie" Text] NoContent)
clientPostStripeHook :: JSON.Value -> ClientM NoContent
clientPostAddItem :: Token -> TypedItem -> ClientM ItemUUID
clientPostSync ::
  Token ->
  SyncRequest Int64 ItemUUID (AddedItem TypedItem) ->
  ClientM (SyncResponse Int64 ItemUUID (AddedItem TypedItem))
clientPostRegister
  :<|> clientPostLogin
  :<|> clientPostStripeHook
  :<|> clientPostAddItem
  :<|> clientPostSync =
    client (flatten intrayAPI)
