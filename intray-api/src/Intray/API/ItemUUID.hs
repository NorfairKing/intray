module Intray.API.ItemUUID
  ( ItemUUID,
    module Data.UUID.Typed,
  )
where

import Data.UUID.Typed
import Intray.API.UUID ()

type ItemUUID = UUID Item

data Item
