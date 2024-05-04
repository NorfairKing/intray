{-# LANGUAGE TypeApplications #-}

module Intray.API.InstanceSpec
  ( spec,
  )
where

import Intray.API.Gen ()
import Intray.API.Types
import Test.Syd.Validity.Aeson
import TestImport

spec :: Spec
spec = do
  genValidSpec @Registration
  jsonSpec @Registration
  genValidSpec @LoginForm
  jsonSpec @LoginForm
  genValidSpec @Pricing
  jsonSpec @Pricing
