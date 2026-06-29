{-# LANGUAGE TypeApplications #-}

module Intray.API.Protected.Account.InstanceSpec
  ( spec,
  )
where

import Intray.API.Protected.Account.Gen ()
import Intray.API.Protected.Account.Types
import TestImport

spec :: Spec
spec = do
  genValidSpec @AccountInfo
  genValidSpec @ChangePassphrase
