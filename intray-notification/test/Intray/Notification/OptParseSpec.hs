{-# LANGUAGE TypeApplications #-}

module Intray.Notification.OptParseSpec (spec) where

import Intray.Notification.OptParse
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  describe "Settings" $ do
    settingsLintSpec @Settings
    goldenSettingsReferenceDocumentationSpec @Settings "test_resources/documentation.txt" "intray"
    goldenSettingsNixOptionsSpec @Settings "options.nix"
