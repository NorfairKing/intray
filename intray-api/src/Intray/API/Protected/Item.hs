{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected.Item
  ( module Intray.API.Protected.Item,
    module Intray.API.Protected.Item.Types,
  )
where

import Data.Int
import Data.Mergeless
import Intray.API.Protected.Item.Types
import Intray.API.Types
import Servant.API

type PostAddItem =
  ProtectAPI
    :> "intray"
    :> "item"
    :> ReqBody '[JSON] TypedItem
    :> Post '[JSON] ItemUUID

type PostSync =
  ProtectAPI
    :> "intray"
    :> "sync"
    :> ReqBody '[JSON] (SyncRequest Int64 ItemUUID (AddedItem TypedItem))
    :> Post '[JSON] (SyncResponse Int64 ItemUUID (AddedItem TypedItem))
