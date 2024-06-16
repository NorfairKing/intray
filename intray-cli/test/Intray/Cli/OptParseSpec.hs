{-# LANGUAGE TypeApplications #-}

module Intray.Cli.OptParseSpec (spec) where

import Intray.Cli.OptParse
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  describe "Settings" $ do
    settingsLintSpec @Settings
    goldenReferenceDocumentationSpec @Settings "test_resources/reference.txt" "intray"
