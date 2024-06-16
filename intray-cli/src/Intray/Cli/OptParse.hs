{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.OptParse
  ( Instructions (..),
    getInstructions,
    Settings (..),
    SyncStrategy (..),
    AutoOpen (..),
    Dispatch (..),
    RegisterSettings (..),
    LoginSettings (..),
    AddSettings (..),
  )
where

import Autodocodec
import Control.Monad.Logger
import Data.Text (Text)
import Intray.API
import OptEnvConf
import Path
import Path.IO
import Paths_intray_cli (version)
import Servant.Client

getInstructions :: IO Instructions
getInstructions = runSettingsParser version "Intray"

data Instructions
  = Instructions !Dispatch !Settings

instance HasParser Instructions where
  settingsParser = parseInstructions

{-# ANN parseInstructions ("NOCOVER" :: String) #-}
parseInstructions :: Parser Instructions
parseInstructions =
  subEnv_ "intray" $
    let configParsers :: Parser [Path Abs File]
        configParsers =
          sequenceA
            [ xdgYamlConfigFile "intray",
              runIO $ do
                homeDir <- getHomeDir
                intrayDir <- resolveDir homeDir ".intray"
                resolveFile intrayDir "config.yaml"
            ]
     in withFirstYamlConfig configParsers $
          Instructions
            <$> settingsParser
            <*> settingsParser

data Settings = Settings
  { setBaseUrl :: Maybe BaseUrl,
    setCacheDir :: Path Abs Dir,
    setDataDir :: Path Abs Dir,
    setSyncStrategy :: SyncStrategy,
    setAutoOpen :: AutoOpen,
    setLogLevel :: LogLevel
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings = do
  setBaseUrl <-
    optional $
      mapIO parseBaseUrl $
        setting
          [ help "api url of the intray server.",
            reader str,
            name "url",
            metavar "URL",
            example "api.intray.eu"
          ]
  setCacheDir <-
    choice
      [ directoryPathSetting
          [ help "directory to store cache information. You can remove this directory as necessary.",
            name "cache-dir"
          ],
        runIO $ getXdgDir XdgCache (Just [reldir|intray|])
      ]
  setDataDir <-
    choice
      [ directoryPathSetting
          [ help "directory to store data information. Removing this directory could lead to data loss.",
            name "data-dir"
          ],
        runIO $ getXdgDir XdgData (Just [reldir|intray|])
      ]
  setSyncStrategy <- settingsParser
  setAutoOpen <- settingsParser
  setLogLevel <- settingsParser
  pure Settings {..}

data SyncStrategy
  = NeverSync
  | AlwaysSync
  deriving stock (Show, Read)

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

instance HasParser SyncStrategy where
  settingsParser =
    setting
      [ help "sync strategy for non-sync commands.",
        reader auto,
        metavar "SYNC_STRATEGY",
        name "sync-strategy",
        shownExample NeverSync,
        shownExample AlwaysSync,
        value AlwaysSync
      ]

data AutoOpen
  = DontAutoOpen
  | AutoOpenWith FilePath
  deriving (Show)

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

instance HasParser AutoOpen where
  settingsParser =
    choice
      [ setting
          [ help "Don't auto-open links or pictures",
            switch DontAutoOpen,
            long "no-auto-open"
          ],
        AutoOpenWith
          <$> setting
            [ help "Command to use to auto-open",
              reader str,
              option,
              long "auto-open-with",
              metavar "COMMAND"
            ],
        setting
          [ help "How to auto-open",
            confWith' "auto-open" $
              dimapCodec
                Just
                ( \case
                    Nothing -> DontAutoOpen
                    Just ao -> ao
                )
                (codec :: JSONCodec AutoOpen),
            value (AutoOpenWith "xdg-open")
          ]
      ]

data Dispatch
  = DispatchRegister !RegisterSettings
  | DispatchLogin !LoginSettings
  | DispatchAddItem !AddSettings
  | DispatchShowItem
  | DispatchDoneItem
  | DispatchSize
  | DispatchReview
  | DispatchLogout
  | DispatchSync

instance HasParser Dispatch where
  settingsParser = parseDispatch

{-# ANN parseDispatch ("NOCOVER" :: String) #-}
parseDispatch :: Parser Dispatch
parseDispatch =
  commands
    [ command "register" "Register with the sync server" $ DispatchRegister <$> settingsParser,
      command "login" "Authenticate with the sync server" $ DispatchLogin <$> settingsParser,
      command "add" "Add an intray item" $ DispatchAddItem <$> settingsParser,
      command "show" "Show one intray item" $ pure DispatchShowItem,
      command "done" "Mark the shown intray item as done" $ pure DispatchDoneItem,
      command "size" "Show the number of items in the intray" $ pure DispatchSize,
      command "review" "Review intray items one by one" $ pure DispatchReview,
      command "logout" "Log out with the sync server" $ pure DispatchLogout,
      command "sync" "Synchronise with the sync server" $ pure DispatchSync
    ]

data RegisterSettings = RegisterSettings
  { registerSetUsername :: !(Maybe Username),
    registerSetPassword :: !(Maybe Text)
  }

instance HasParser RegisterSettings where
  settingsParser = parseRegisterSettings

{-# ANN parseRegisterSettings ("NOCOVER" :: String) #-}
parseRegisterSettings :: Parser RegisterSettings
parseRegisterSettings = do
  registerSetUsername <- parseUsernameSetting
  registerSetPassword <- parsePasswordSetting
  pure RegisterSettings {..}

data LoginSettings = LoginSettings
  { loginSetUsername :: !(Maybe Username),
    loginSetPassword :: !(Maybe Text)
  }

instance HasParser LoginSettings where
  settingsParser = parseLoginSettings

{-# ANN parseLoginSettings ("NOCOVER" :: String) #-}
parseLoginSettings :: Parser LoginSettings
parseLoginSettings = do
  loginSetUsername <- parseUsernameSetting
  loginSetPassword <- parsePasswordSetting
  pure LoginSettings {..}

parseUsernameSetting :: Parser (Maybe Username)
parseUsernameSetting =
  optional
    ( checkMapEither parseUsernameWithError $
        setting
          [ help "Username",
            reader str,
            metavar "USERNAME",
            name "username"
          ]
    )

parsePasswordSetting :: Parser (Maybe Text)
parsePasswordSetting =
  optional
    ( choice
        [ mapIO readSecretTextFile $
            filePathSetting
              [ help "Password file",
                name "password-file"
              ],
          setting
            [ help "Password",
              reader str,
              metavar "PASSWORD",
              name "password"
            ]
        ]
    )

data AddSettings = AddSettings
  { addSetContents :: ![Text],
    addSetReadStdin :: !Bool,
    addSetRemote :: !Bool
  }

instance HasParser AddSettings where
  settingsParser = parseAddSettings

{-# ANN parseAddSettings ("NOCOVER" :: String) #-}
parseAddSettings :: Parser AddSettings
parseAddSettings = do
  addSetContents <-
    many $
      setting
        [ help "contents of the items to be added",
          reader str,
          argument,
          metavar "TEXT"
        ]
  addSetReadStdin <-
    setting
      [ help "read contents from standard input too",
        switch True,
        value False,
        long "stdin"
      ]
  addSetRemote <-
    setting
      [ help "only add the item remotely, not locally. This implies --sync-strategy NeverSync",
        switch True,
        value False,
        long "remote"
      ]
  pure AddSettings {..}
