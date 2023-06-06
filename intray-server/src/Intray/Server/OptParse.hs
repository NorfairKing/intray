{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Intray.Server.OptParse
  ( module Intray.Server.OptParse,
    module Intray.Server.OptParse.Types,
  )
where

import Autodocodec.Yaml
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist.Sqlite
import qualified Env
import Import
import Intray.API
import Intray.Server.OptParse.Types
import Options.Applicative
import qualified Options.Applicative.Help as OptParse
import qualified System.Environment as System

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings flags@Flags {..} env@Environment {..} mConf = do
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc func = mConf >>= func
  let setLogLevel = fromMaybe LevelInfo $ flagLogLevel <|> envLogLevel <|> mc confLogLevel
  when (setLogLevel >= LevelDebug) $ pPrint (flags, env, mConf)
  let setPort = fromMaybe 8000 $ flagPort <|> envPort <|> mc confPort
  let setHost =
        T.pack $ fromMaybe ("localhost:" <> show setPort) $ flagHost <|> envHost <|> mc confHost
  setSigningKeyFile <-
    case flagSigningKeyFile <|> envSigningKeyFile <|> mc confSigningKeyFile of
      Nothing -> resolveFile' "signing-key.json"
      Just skf -> resolveFile' skf
  let setConnectionInfo = mkSqliteConnectionInfo $ fromMaybe "intray.db" (flagDb <|> envDb <|> mc confDb)
  setAdmins <-
    forM (flagAdmins ++ fromMaybe [] (mc confAdmins)) $ \s ->
      case parseUsername $ T.pack s of
        Nothing -> die $ unwords ["Invalid admin username:", s]
        Just u -> pure u
  setFreeloaders <-
    forM (flagFreeloaders ++ fromMaybe [] (mc confFreeloaders)) $ \s ->
      case parseUsername $ T.pack s of
        Nothing -> die $ unwords ["Invalid freeloader username:", s]
        Just u -> pure u
  setMonetisationSettings <-
    do
      let mmc :: (MonetisationConfiguration -> Maybe a) -> Maybe a
          mmc func = mc confMonetisationConfig >>= func
      let plan =
            T.pack
              <$> (flagStripePlan <|> envStripePlan <|> mmc monetisationConfStripePlan)
      let secretKey =
            T.pack
              <$> ( flagStripeSecretKey
                      <|> envStripeSecretKey
                      <|> mmc monetisationConfStripeSecretKey
                  )
      let publicKey =
            T.pack
              <$> ( flagStripePublishableKey
                      <|> envStripePublishableKey
                      <|> mmc monetisationConfStripePublishableKey
                  )
      let maxItemsFree =
            fromMaybe 5 $
              flagMaxItemsFree <|> envMaxItemsFree <|> mmc monetisationConfMaxItemsFree
      pure $ do
        ss <- StripeSettings <$> plan <*> secretKey <*> publicKey
        price <- flagPrice <|> envPrice <|> mmc monetisationConfPrice
        pure $
          MonetisationSettings
            { monetisationSetStripeSettings = ss,
              monetisationSetMaxItemsFree = maxItemsFree,
              monetisationSetPrice = price
            }
  pure Settings {..}

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} = do
  cp <-
    case flagConfigFile <|> envConfigFile of
      Nothing -> getDefaultConfigFile
      Just cf -> resolveFile' cf
  readYamlConfigFile cp

getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = resolveFile' "config.yaml"

getEnvironment :: IO Environment
getEnvironment = Env.parse id environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "INTRAY_SERVER_" $
    Environment
      <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Config file"))
      <*> optional (Env.var Env.str "HOST" (Env.help "host to run the api server on"))
      <*> optional (Env.var Env.auto "PORT" (Env.help "port to run the api server on"))
      <*> optional (Env.var Env.str "DATABASE" (Env.help "database file"))
      <*> optional (Env.var Env.auto "LOG_LEVEL" (Env.help "minimal severity of log messages"))
      <*> optional (Env.var Env.str "SIGNING_KEY_FILE" (Env.help "the file to store the signing key in"))
      <*> optional (Env.var Env.str "STRIPE_PLAN" (Env.help "stripe plan id for subscriptions"))
      <*> optional (Env.var Env.str "STRIPE_SECRET_KEY" (Env.help "stripe secret key"))
      <*> optional (Env.var Env.str "STRIPE_PUBLISHABLE_KEY" (Env.help "stripe publishable key"))
      <*> optional (Env.var Env.auto "MAX_ITEMS_FREE" (Env.help "maximum items that a free user can have"))
      <*> optional (Env.var Env.str "PRICE" (Env.help "A text description of the plan price"))

getFlags :: IO Flags
getFlags = do
  args <- System.getArgs
  let result = runFlagsParser args
  handleParseResult result

runFlagsParser :: [String] -> ParserResult Flags
runFlagsParser = execParserPure prefs_ flagsParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

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
    <*> option
      (Just <$> str)
      (mconcat [long "host", value Nothing, metavar "HOST", help "the host to serve on"])
    <*> option
      (Just <$> auto)
      (mconcat [long "port", value Nothing, metavar "PORT", help "the port to serve on"])
    <*> option
      (Just . T.pack <$> str)
      ( mconcat
          [ long "database",
            value Nothing,
            metavar "DATABASE_CONNECTION_STRING",
            help "The sqlite connection string"
          ]
      )
    <*> many (strOption (mconcat [long "admin", metavar "USERNAME", help "An admin"]))
    <*> many (strOption (mconcat [long "freeloader", metavar "USERNAME", help "A freeloader"]))
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
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "signing-key-file",
            value Nothing,
            metavar "FILEPATH",
            help "the file to store the signing key in"
          ]
      )
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "stripe-plan",
            value Nothing,
            metavar "PLAN_ID",
            help "The product pricing plan for stripe"
          ]
      )
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "stripe-secret-key",
            value Nothing,
            metavar "SECRET_KEY",
            help "The secret key for stripe"
          ]
      )
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "stripe-publishable-key",
            value Nothing,
            metavar "PUBLISHABLE_KEY",
            help "The publishable key for stripe"
          ]
      )
    <*> option
      (Just <$> auto)
      ( mconcat
          [ long "max-items-free",
            value Nothing,
            metavar "INT",
            help "How many items a user can sync in the free plan"
          ]
      )
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "price",
            value Nothing,
            metavar "PRICE",
            help "A text description of the price"
          ]
      )
