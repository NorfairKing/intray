{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Data.Text (Text)
import Intray.API
import Path
import Servant.Client

data Arguments
  = Arguments Command Flags

data Instructions
  = Instructions Dispatch Settings

data Command
  = CommandRegister RegisterArgs
  | CommandLogin LoginArgs
  | CommandAddItem AddArgs
  | CommandShowItem
  | CommandDoneItem
  | CommandSize
  | CommandReview
  | CommandLogout
  | CommandSync

data RegisterArgs = RegisterArgs
  { registerArgUsername :: Maybe String,
    registerArgPassword :: Maybe String,
    registerArgPasswordFile :: Maybe FilePath
  }

data LoginArgs = LoginArgs
  { loginArgUsername :: Maybe String,
    loginArgPassword :: Maybe String,
    loginArgPasswordFile :: Maybe FilePath
  }

data AddArgs = AddArgs
  { addArgContents :: [String],
    addArgReadStdin :: Bool,
    addArgRemote :: Bool
  }

data Flags = Flags
  { flagConfigFile :: Maybe FilePath,
    flagUrl :: Maybe String,
    flagCacheDir :: Maybe FilePath,
    flagDataDir :: Maybe FilePath,
    flagSyncStrategy :: Maybe SyncStrategy,
    flagAutoOpen :: Maybe AutoOpen,
    flagLogLevel :: Maybe LogLevel
  }

data Environment = Environment
  { envConfigFile :: Maybe FilePath,
    envUrl :: Maybe String,
    envUsername :: Maybe String,
    envPassword :: Maybe String,
    envPasswordFile :: Maybe FilePath,
    envCacheDir :: Maybe FilePath,
    envDataDir :: Maybe FilePath,
    envSyncStrategy :: Maybe SyncStrategy,
    envAutoOpen :: Maybe AutoOpen,
    envLogLevel :: Maybe LogLevel
  }

data Configuration = Configuration
  { configUrl :: Maybe String,
    configUsername :: Maybe String,
    configPassword :: Maybe String,
    configPasswordFile :: Maybe FilePath,
    configCacheDir :: Maybe FilePath,
    configDataDir :: Maybe FilePath,
    configSyncStrategy :: Maybe SyncStrategy,
    configAutoOpen :: Maybe AutoOpen,
    configLogLevel :: Maybe LogLevel
  }

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldOrNull "url" "The api url of the intray server. Example: api.intray.eu" .= configUrl
        <*> optionalFieldOrNull "username" "The username to log in with" .= configUsername
        <*> optionalFieldOrNull
          "password"
          "The password to log in with. Note that leaving your password in plaintext in a config file is not safe. Only use this for automation."
          .= configPassword
        <*> optionalFieldOrNull
          "password-file"
          "The path to a file containing the password to log in with. Note that leaving your password in plaintext in a config file is not safe. Only use this for automation."
          .= configPasswordFile
        <*> optionalFieldOrNull
          "cache-dir"
          "The directory to store cache information. You can remove this directory as necessary."
          .= configCacheDir
        <*> optionalFieldOrNull
          "data-dir"
          "The directory to store data information. Removing this directory could lead to data loss."
          .= configDataDir
        <*> optionalFieldOrNull "sync" "The sync strategy for non-sync commands." .= configSyncStrategy
        <*> optionalFieldOrNull "auto-open" "how to auto-open" .= configAutoOpen
        <*> optionalFieldOrNull "log-level" "minimal severity for log message" .= configLogLevel

data Settings = Settings
  { setBaseUrl :: Maybe BaseUrl,
    setCacheDir :: Path Abs Dir,
    setDataDir :: Path Abs Dir,
    setSyncStrategy :: SyncStrategy,
    setAutoOpen :: AutoOpen,
    setLogLevel :: LogLevel
  }

data SyncStrategy
  = NeverSync
  | AlwaysSync
  deriving stock (Read)

instance HasCodec SyncStrategy where
  codec =
    dimapCodec f g $
      eitherCodec
        ( literalTextValueCodec NeverSync "NeverSync"
            <??> [ "Only sync when manually running 'intray sync'.",
                   "When using this option, you essentially promise that you will take care of ensuring that syncing happens regularly."
                 ]
        )
        ( literalTextValueCodec AlwaysSync "AlwaysSync"
            <??> [ "Sync on every change to the local state.",
                   "Commands will still succeed even if the sync fails because of internet connect problems for example."
                 ]
        )
    where
      f = \case
        Left ns -> ns
        Right as -> as
      g = \case
        NeverSync -> Left NeverSync
        AlwaysSync -> Right AlwaysSync

data AutoOpen = DontAutoOpen | AutoOpenWith FilePath

instance HasCodec AutoOpen where
  codec =
    dimapCodec f g $
      eitherCodec
        (nullCodec <?> "Explicitly _don't_ auto-open links or pictures.")
        (codec <?> "Auto-open with the given command. xdg-open is the default.")
    where
      f = \case
        Left () -> DontAutoOpen
        Right s -> AutoOpenWith s
      g = \case
        DontAutoOpen -> Left ()
        AutoOpenWith s -> Right s

data Dispatch
  = DispatchRegister RegisterSettings
  | DispatchLogin LoginSettings
  | DispatchAddItem AddSettings
  | DispatchShowItem
  | DispatchDoneItem
  | DispatchSize
  | DispatchReview
  | DispatchLogout
  | DispatchSync

data RegisterSettings = RegisterSettings
  { registerSetUsername :: Maybe Username,
    registerSetPassword :: Maybe Text
  }

data LoginSettings = LoginSettings
  { loginSetUsername :: Maybe Username,
    loginSetPassword :: Maybe Text
  }

data AddSettings = AddSettings
  { addSetContents :: [Text],
    addSetReadStdin :: Bool,
    addSetRemote :: Bool
  }
