{-# LANGUAGE TypeApplications #-}

module Intray.Cli.OptParseSpec (spec) where

import Intray.Cli.OptParse
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  describe "Intructions" $ do
    settingsLintSpec @Instructions
    goldenSettingsReferenceDocumentationSpec @Instructions "test_resources/documentation.txt" "intray"
    goldenSettingsNixOptionsSpec @Instructions "options.nix"
