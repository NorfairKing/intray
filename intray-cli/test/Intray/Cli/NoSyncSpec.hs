{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Intray.Cli.NoSyncSpec
  ( spec,
  )
where

import Intray.API
import Intray.Cli.OptParse
import Intray.Cli.TestUtils
import TestImport

spec :: Spec
spec = offlineCliMSpec $ do
  it "correctly errors when a user tries to register but has no server configured" $ \settings ->
    testIntray
      settings
      ( DispatchRegister $
          RegisterSettings
            { registerSetUsername = parseUsername "testuser",
              registerSetPassword = Just "password"
            }
      )
      `shouldThrow` (\(_ :: ExitCode) -> True)

  it "correctly errors when a user tries to login but has no server configured" $ \settings ->
    testIntray
      settings
      ( DispatchLogin $
          LoginSettings
            { loginSetUsername = parseUsername "testuser",
              loginSetPassword = Just "password"
            }
      )
      `shouldThrow` (\(_ :: ExitCode) -> True)

  it "Works fine without a server" $ \settings -> do
    let intray = testIntray settings
    intray $
      DispatchAddItem $
        AddSettings
          { addSetContents = ["hello", "world"],
            addSetReadStdin = False,
            addSetRemote = False
          }
    intray DispatchShowItem
    intray DispatchDoneItem
    intray DispatchSize
