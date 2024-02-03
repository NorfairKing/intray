{-# LANGUAGE RecordWildCards #-}

module Intray.Web.Server
  ( intrayWebServer,
  )
where

import Control.Monad.Logger
import Import
import Intray.Web.Server.Application ()
import Intray.Web.Server.Foundation
import Intray.Web.Server.OptParse
import qualified Network.HTTP.Client.TLS as Http
import Yesod

intrayWebServer :: IO ()
intrayWebServer = do
  settings <- getSettings
  pPrint settings
  runIntrayWebServer settings

runIntrayWebServer :: Settings -> IO ()
runIntrayWebServer Settings {..} =
  runStderrLoggingT
    $ filterLogger (\_ ll -> ll >= setLogLevel)
    $ do
      man <- liftIO Http.newTlsManager
      let app =
            App
              { appHttpManager = man,
                appStatic = myStatic,
                appTracking = setTracking,
                appVerification = setVerification,
                appAPIBaseUrl = setAPIBaseUrl
              }
      liftIO $ do
        warp setPort app
