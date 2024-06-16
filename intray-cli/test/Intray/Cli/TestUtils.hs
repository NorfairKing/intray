{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.TestUtils where

import Intray.Cli
import Intray.Cli.OptParse
import qualified Network.HTTP.Client as HTTP
import Test.Syd.Path
import Test.Syd.Wai
import TestImport

type CliSpec = TestDef '[HTTP.Manager] Settings

testIntray :: Settings -> Dispatch -> IO ()
testIntray settings d = dispatch (Instructions d settings)

offlineCliMSpec :: CliSpec -> Spec
offlineCliMSpec = managerSpec . setupAround offlineEnvSetupFunc

offlineEnvSetupFunc :: SetupFunc Settings
offlineEnvSetupFunc = do
  tempDir <- tempDirSetupFunc "intray-cli-test-dir"
  setCacheDir <- resolveDir tempDir "cache"
  setDataDir <- resolveDir tempDir "data"
  let setSyncStrategy = NeverSync
  let setAutoOpen = DontAutoOpen
  let setLogLevel = LevelError
  let setBaseUrl = Nothing
  pure Settings {..}
