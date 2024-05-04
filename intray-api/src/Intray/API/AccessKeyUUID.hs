module Intray.API.AccessKeyUUID
  ( AccessKeyUUID,
    module Data.UUID.Typed,
  )
where

import Data.UUID.Typed
import Intray.API.UUID ()

type AccessKeyUUID = UUID AccessKey

data AccessKey
