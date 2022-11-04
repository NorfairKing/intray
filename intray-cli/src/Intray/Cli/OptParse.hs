{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

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

import Autodocodec.Yaml
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Env
import Import
import Intray.Cli.OptParse.Types
import Intray.Data
import Options.Applicative
import qualified Options.Applicative.Help as OptParse
import Servant.Client
import qualified System.Environment as System

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments cmd Flags {..}) Environment {..} mConf =
  Instructions <$> getDispatch <*> getSettings
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f
    getSettings = do
      setBaseUrl <-
        case flagUrl <|> envUrl <|> mc configUrl of
          Nothing -> pure Nothing
          Just url -> Just <$> parseBaseUrl url
      setCacheDir <-
        case flagCacheDir <|> envCacheDir <|> mc configCacheDir of
          Nothing -> getXdgDir XdgCache (Just [reldir|intray|])
          Just d -> resolveDir' d
      setDataDir <-
        case flagDataDir <|> envDataDir <|> mc configDataDir of
          Nothing -> getXdgDir XdgData (Just [reldir|intray|])
          Just d -> resolveDir' d
      let setSyncStrategy =
            fromMaybe
              ( case setBaseUrl of
                  Nothing -> NeverSync
                  Just _ -> AlwaysSync
              )
              $ flagSyncStrategy <|> envSyncStrategy <|> mc configSyncStrategy
      let setAutoOpen = fromMaybe (AutoOpenWith "xdg-open") (flagAutoOpen <|> envAutoOpen <|> mc configAutoOpen)
      let setLogLevel = fromMaybe LevelWarn (flagLogLevel <|> envLogLevel <|> mc configLogLevel)
      pure Settings {..}
    mPass mPassword mPasswordFile = case mPassword of
      Just password -> pure $ Just (T.pack password)
      Nothing -> case mPasswordFile of
        Nothing -> pure Nothing
        Just passwordFile -> Just . T.strip <$> T.readFile passwordFile
    getDispatch =
      case cmd of
        CommandRegister RegisterArgs {..} -> do
          flagMPass <- mPass registerArgPassword registerArgPasswordFile
          envMPass <- mPass envPassword envPasswordFile
          confMPass <- mPass (mc configPassword) (mc configPasswordFile)
          pure $
            DispatchRegister
              RegisterSettings
                { registerSetUsername =
                    (T.pack <$> (registerArgUsername <|> envUsername <|> mc configUsername))
                      >>= parseUsername,
                  registerSetPassword = flagMPass <|> envMPass <|> confMPass
                }
        CommandLogin LoginArgs {..} -> do
          flagMPass <- mPass loginArgPassword loginArgPasswordFile
          envMPass <- mPass envPassword envPasswordFile
          confMPass <- mPass (mc configPassword) (mc configPasswordFile)
          pure $
            DispatchLogin
              LoginSettings
                { loginSetUsername =
                    (T.pack <$> (loginArgUsername <|> envUsername <|> mc configUsername))
                      >>= parseUsername,
                  loginSetPassword =
                    flagMPass <|> envMPass <|> confMPass
                }
        CommandAddItem AddArgs {..} ->
          pure $
            DispatchAddItem
              AddSettings
                { addSetContents = map T.pack addArgContents,
                  addSetReadStdin = addArgReadStdin,
                  addSetRemote = addArgRemote
                }
        CommandShowItem -> pure DispatchShowItem
        CommandDoneItem -> pure DispatchDoneItem
        CommandSize -> pure DispatchSize
        CommandReview -> pure DispatchReview
        CommandLogout -> pure DispatchLogout
        CommandSync -> pure DispatchSync

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFiles >>= readFirstYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

defaultConfigFiles :: IO [Path Abs File]
defaultConfigFiles =
  sequence
    [ do
        xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|intray|])
        resolveFile xdgConfigDir "config.yaml",
      do
        homeDir <- getHomeDir
        intrayDir <- resolveDir homeDir ".intray"
        resolveFile intrayDir "config.yaml"
    ]

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "INTRAY_" $
    Environment
      <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Config file"))
      <*> optional (Env.var Env.str "URL" (Env.help "sync server url"))
      <*> optional (Env.var Env.str "USERNAME" (Env.help "Sync username"))
      <*> optional (Env.var Env.str "PASSWORD" (Env.help "Sync password"))
      <*> optional (Env.var Env.str "PASSWORD_FILE" (Env.help "Sync password file"))
      <*> optional (Env.var Env.str "CACHE_DIR" (Env.help "cache directory"))
      <*> optional (Env.var Env.str "DATA_DIR" (Env.help "data directory"))
      <*> optional (Env.var Env.auto "SYNC_STRATEGY" (Env.help "Sync strategy"))
      <*> optional (Env.var (fmap AutoOpenWith . Env.str) "AUTO_OPEN" (Env.help "The command to auto-open links and pictures"))
      <*> optional (Env.var Env.auto "LOG_LEVEL" (Env.help "minimal severity of log messages"))

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser

prefs_ :: ParserPrefs
prefs_ = defaultPrefs {prefShowHelpOnEmpty = True, prefShowHelpOnError = True}

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) (fullDesc <> footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (renderColouredSchemaViaCodec @Configuration)
        ]

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ command "register" parseCommandRegister,
        command "login" parseCommandLogin,
        command "add" parseCommandPostPostAddItem,
        command "show" parseCommandShowItem,
        command "done" parseCommandDoneItem,
        command "size" parseCommandSize,
        command "review" parseCommandReview,
        command "logout" parseCommandLogout,
        command "sync" parseCommandSync
      ]

parseCommandRegister :: ParserInfo Command
parseCommandRegister = info parser modifier
  where
    modifier = fullDesc <> progDesc "Register user"
    parser =
      CommandRegister
        <$> ( RegisterArgs
                <$> parseUsernameOption
                <*> parsePasswordOption
                <*> parsePasswordFileOption
            )

parseCommandLogin :: ParserInfo Command
parseCommandLogin = info parser modifier
  where
    modifier = fullDesc <> progDesc "Login user"
    parser =
      CommandLogin
        <$> ( LoginArgs
                <$> parseUsernameOption
                <*> parsePasswordOption
                <*> parsePasswordFileOption
            )

parseUsernameOption :: Parser (Maybe String)
parseUsernameOption =
  optional $
    strOption
      ( mconcat
          [ long "username",
            help "The username to register",
            metavar "USERNAME"
          ]
      )

parsePasswordOption :: Parser (Maybe String)
parsePasswordOption =
  optional $
    strOption
      ( mconcat
          [ long "password",
            help "The password to register with. If both password and password-file are absent, a prompt will ask for the password.",
            metavar "PASSWORD"
          ]
      )

parsePasswordFileOption :: Parser (Maybe FilePath)
parsePasswordFileOption =
  optional $
    strOption
      ( mconcat
          [ long "password-file",
            help "The path to the password to register with. If both password and password-file are absent, a prompt will ask for the password.",
            metavar "PASSWORD"
          ]
      )

parseCommandPostPostAddItem :: ParserInfo Command
parseCommandPostPostAddItem = info parser modifier
  where
    modifier = fullDesc <> progDesc "Add an item"
    parser =
      CommandAddItem
        <$> ( AddArgs
                <$> many
                  (strArgument (mconcat [help "Give the contents of the item to be added.", metavar "TEXT"]))
                <*> switch (mconcat [long "stdin", help "Read contents from stdin too"])
                <*> switch
                  ( mconcat
                      [ long "remote",
                        help "Only add the item remotely, not locally. This implies --sync-strategy NeverSync"
                      ]
                  )
            )

parseCommandShowItem :: ParserInfo Command
parseCommandShowItem = info parser modifier
  where
    modifier = fullDesc <> progDesc "Show one item."
    parser = pure CommandShowItem

parseCommandDoneItem :: ParserInfo Command
parseCommandDoneItem = info parser modifier
  where
    modifier = fullDesc <> progDesc "Mark that item as done."
    parser = pure CommandDoneItem

parseCommandSize :: ParserInfo Command
parseCommandSize = info parser modifier
  where
    modifier = fullDesc <> progDesc "Show the number of items in the intray."
    parser = pure CommandSize

parseCommandReview :: ParserInfo Command
parseCommandReview = info parser modifier
  where
    modifier = fullDesc <> progDesc "Start reviewing items one by one."
    parser = pure CommandReview

parseCommandLogout :: ParserInfo Command
parseCommandLogout = info parser modifier
  where
    modifier = fullDesc <> progDesc "Logout user"
    parser = pure CommandLogout

parseCommandSync :: ParserInfo Command
parseCommandSync = info parser modifier
  where
    modifier = fullDesc <> progDesc "Sync the local and remote intray"
    parser = pure CommandSync

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> option
      (Just <$> str)
      ( mconcat
          [ long "config-file",
            help "Give the path to an altenative config file",
            value Nothing,
            metavar "FILEPATH"
          ]
      )
    <*> option
      (Just <$> str)
      (mconcat [long "url", help "The url of the server.", value Nothing, metavar "URL"])
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "cache-dir",
            help "The directory to use for caching",
            value Nothing,
            metavar "FILEPATH"
          ]
      )
    <*> option
      (Just <$> str)
      ( mconcat
          [long "data-dir", help "The directory to use for data", value Nothing, metavar "FILEPATH"]
      )
    <*> syncStrategyOpt
    <*> autoOpenOpt
    <*> option
      (Just <$> auto)
      ( mconcat
          [ long "log-level",
            metavar "LOG_LEVEL",
            value Nothing,
            help $
              "the log level, possible values: " <> show [LevelDebug, LevelInfo, LevelWarn, LevelError]
          ]
      )

syncStrategyOpt :: Parser (Maybe SyncStrategy)
syncStrategyOpt =
  flag Nothing (Just NeverSync) (mconcat [long "no-sync", help "Do not try to sync."])
    <|> flag Nothing (Just AlwaysSync) (mconcat [long "sync", help "Definitely try to sync."])

autoOpenOpt :: Parser (Maybe AutoOpen)
autoOpenOpt =
  flag Nothing (Just DontAutoOpen) (mconcat [long "no-auto-open", help "Do not try to open links and pictures."])
    <|> (Just . AutoOpenWith <$> strOption (mconcat [long "auto-open-with", help "The command to use to auto-open links and pictures. The default is xdg-open."]))
