module Intray.API.AccountUUID
  ( AccountUUID,
    module Data.UUID.Typed,
  )
where

import Data.UUID.Typed
import Intray.API.UUID ()

type AccountUUID = UUID User

data User
