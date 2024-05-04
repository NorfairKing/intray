{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected.Gen
  ( module Intray.API.Protected.Item.Gen,
    module Intray.API.Protected.Account.Gen,
  )
where

import Intray.API.Protected.Account.Gen ()
import Intray.API.Protected.Item.Gen ()
