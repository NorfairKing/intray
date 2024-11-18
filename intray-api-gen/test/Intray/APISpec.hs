{-# LANGUAGE TypeApplications #-}

module Intray.APISpec (spec) where

import Intray.API
import Intray.API.Gen ()
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
  genValidSpec @AdminStats
  jsonSpec @AdminStats
  genValidSpec @AccountInfo
  jsonSpec @AccountInfo
  genValidSpec @ChangePassphrase
  jsonSpec @ChangePassphrase
  eqSpec @(ItemInfo ByteString)
  ordSpec @(ItemInfo ByteString)
  genValidSpec @(ItemInfo ByteString)
  eqSpec @TypedItem
  ordSpec @TypedItem
  genValidSpec @TypedItem
  jsonSpec @TypedItem
  eqSpec @(ItemInfo TypedItem)
  ordSpec @(ItemInfo TypedItem)
  genValidSpec @(ItemInfo TypedItem)
  jsonSpec @(ItemInfo TypedItem)
