{-# LANGUAGE TypeApplications #-}

module Intray.API.Protected.Item.InstanceSpec
  ( spec,
  )
where

import Intray.API.Protected.Item.Gen ()
import Intray.API.Protected.Item.Types
import Test.Syd.Validity.Aeson
import TestImport

spec :: Spec
spec = do
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
