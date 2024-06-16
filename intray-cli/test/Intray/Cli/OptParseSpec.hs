{-# LANGUAGE TypeApplications #-}

module Intray.Cli.OptParseSpec (spec) where

import Intray.Cli.OptParse
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  describe "Intructions" $ do
    settingsLintSpec @Instructions
    goldenReferenceDocumentationSpec @Instructions "test_resources/reference.txt" "intray"
