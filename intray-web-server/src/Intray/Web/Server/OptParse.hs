{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Intray.Web.Server.OptParse
  ( getSettings,
    Settings (..),
  )
where

import Autodocodec.Yaml
import Control.Arrow
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Env
import Import
import Intray.Web.Server.OptParse.Types
import Options.Applicative
import qualified Options.Applicative.Help as OptParse
import Servant.Client
import qualified System.Environment as System

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc func = mConf >>= func
  let setPort = fromMaybe 8080 $ flagPort <|> envPort <|> mc confPort
  setAPIBaseUrl <- case flagAPIBaseUrl <|> envAPIBaseUrl <|> mc confAPIBaseUrl of
    Nothing -> die "No API URL Configured. Try --help to see how to configure it."
    Just burl -> pure burl
  let setLogLevel = fromMaybe LevelInfo $ flagLogLevel <|> envLogLevel <|> mc confLogLevel
  let setTracking = flagTracking <|> envTracking <|> mc confTracking
  let setVerification = flagVerification <|> envVerification <|> mc confVerification
  pure Settings {..}

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} = do
  configFile <- case flagConfigFile <|> envConfigFile of
    Nothing -> getDefaultConfigFile
    Just cf -> resolveFile' cf
  readYamlConfigFile configFile

getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = resolveFile' "config.yaml"

getEnvironment :: IO Environment
getEnvironment = Env.parse id environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "INTRAY_WEB_SERVER_" $
    Environment
      <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Config file"))
      <*> optional (Env.var Env.auto "PORT" (Env.help "port to run the web server on"))
      <*> optional (Env.var Env.auto "LOG_LEVEL" (Env.help "minimal severity for log messages"))
      <*> optional (Env.var (left (Env.UnreadError . show) . parseBaseUrl) "API_URL" (Env.help "base url for the api server to call"))
      <*> optional (Env.var Env.str "ANALYTICS_TRACKING_ID" (Env.help "google analytics tracking id"))
      <*> optional (Env.var Env.str "SEARCH_CONSOLE_VERIFICATION" (Env.help "google search console verification id"))

getFlags :: IO Flags
getFlags = do
  args <- System.getArgs
  let result = runFlagsParser args
  handleParseResult result

runFlagsParser :: [String] -> ParserResult Flags
runFlagsParser = execParserPure prefs_ flagsParser
  where
    prefs_ = defaultPrefs {prefShowHelpOnError = True, prefShowHelpOnEmpty = True}

flagsParser :: ParserInfo Flags
flagsParser = info (helper <*> parseFlags) (fullDesc <> footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (renderColouredSchemaViaCodec @Configuration)
        ]

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> option
      (Just <$> str)
      (mconcat [long "config-file", value Nothing, metavar "FILEPATH", help "The config file"])
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "port",
                metavar "PORT",
                help "the port to serve on"
              ]
          )
      )
    <*> optional
      ( option
          (eitherReader $ left show . parseBaseUrl)
          ( mconcat
              [ long "api-url",
                metavar "URL",
                help "the url to call the API server on"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "log-level",
                metavar "LOG_LEVEL",
                help "the minimal severity for log messages"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "analytics-tracking-id",
                metavar "TRACKING_ID",
                help "The google analytics tracking ID"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "search-console-verification",
                metavar "VERIFICATION_TAG",
                help "The contents of the google search console verification tag"
              ]
          )
      )
