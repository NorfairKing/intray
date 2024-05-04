{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Commands.Login (login) where

import Control.Monad.IO.Class
import Data.List (find)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Intray.API
import Intray.Cli.Client
import Intray.Cli.Env
import Intray.Cli.OptParse
import Intray.Cli.Prompt
import Intray.Cli.Session
import Intray.Client
import System.Exit
import Web.Cookie (parseSetCookie, setCookieName)

login :: LoginSettings -> CliM ()
login LoginSettings {..} = do
  loginForm <-
    LoginForm
      <$> promptUsername loginSetUsername
      <*> promptPassword loginSetPassword

  mRes <- runSingleClientOrErr $ clientPostLogin loginForm
  case mRes of
    Nothing -> liftIO $ die "No server configured."
    Just (Headers NoContent (HCons sessionHeader HNil)) ->
      case sessionHeader of
        MissingHeader ->
          liftIO $ die "The server responded but the response was missing the right session header."
        UndecodableHeader _ ->
          liftIO $ die "The server responded but the response had an undecodable session header."
        Header setCookieText -> do
          let cookies = parseSetCookie . encodeUtf8 <$> T.lines setCookieText
              jwtCookie = find ((== "JWT-Cookie") . setCookieName) cookies
          case jwtCookie of
            Nothing -> liftIO $ die "No JWT-Cookie was found in the Set-Cookie session header."
            Just setCookie -> saveSession setCookie
