{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Notification (intrayNotification) where

import Control.Monad.IO.Class
import Data.List (find, intercalate)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Intray.Client
import Intray.Notification.OptParse
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import System.Exit
import Web.Cookie (parseSetCookie, setCookieName, setCookieValue)

intrayNotification :: IO ()
intrayNotification = getSettings >>= notifyWithSettings

notifyWithSettings :: Settings -> IO ()
notifyWithSettings Settings {..} = do
  man <- liftIO $ HTTP.newManager tlsManagerSettings
  let cenv = mkClientEnv man settingBaseUrl
  errOrRes <- flip runClientM cenv $ do
    loginRes <- clientPostLogin settingLoginForm
    case loginRes of
      Headers NoContent (HCons sessionHeader HNil) ->
        case sessionHeader of
          MissingHeader ->
            liftIO $ die "The server responded but the response was missing the right session header."
          UndecodableHeader _ ->
            liftIO $ die "The server responded but the response had an undecodable session header."
          Header setCookieText -> do
            let cookies = parseSetCookie . TE.encodeUtf8 <$> T.lines setCookieText
                jwtCookie = find ((== "JWT-Cookie") . setCookieName) cookies
            case jwtCookie of
              Nothing -> liftIO $ die "Didn't get a JWT-Cookie"
              Just cookie -> do
                let token = Token $ setCookieValue cookie
                stdinContents <- liftIO getContents
                let contents = T.strip $ T.pack $ intercalate "\n" [settingContents, stdinContents]
                if T.null contents
                  then pure ()
                  else do
                    _ <- clientPostAddItem token $ textTypedItem contents
                    pure ()
  case errOrRes of
    Left err -> die $ show err
    Right _ -> pure ()
