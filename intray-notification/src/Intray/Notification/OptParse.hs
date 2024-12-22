{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Notification.OptParse
  ( getSettings,
    Settings (..),
  )
where

import qualified Data.Text as T
import Intray.API
import OptEnvConf
import Paths_intray_notification (version)
import Servant.Client

getSettings :: IO Settings
getSettings = runSettingsParser version "Intray Notification"

data Settings = Settings
  { settingBaseUrl :: !BaseUrl,
    settingLoginForm :: !LoginForm,
    settingContents :: !String
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings = do
  settingBaseUrl <-
    mapIO parseBaseUrl $
      setting
        [ help "api url of the intray server.",
          option,
          reader str,
          long "url",
          metavar "URL",
          value "https://api.intray.eu"
        ]
  settingLoginForm <-
    LoginForm
      <$> setting
        [ help "Username",
          reader $ eitherReader $ parseUsernameWithError . T.pack,
          metavar "USERNAME",
          option,
          long "username"
        ]
      <*> secretTextFileOrBareSetting
        [ help "Password",
          option,
          long "password",
          long "key"
        ]
  settingContents <-
    fmap unwords $
      many $
        setting
          [ help "Contents of the item",
            argument,
            reader str,
            metavar "CONTENTS"
          ]
  pure Settings {..}
