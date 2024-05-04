{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Prompt
  ( promptUsername,
    promptPassword,
    prompt,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text.IO as T
import Intray.API.Username
import Intray.Cli.Env
import System.IO

promptUsername :: Maybe Username -> CliM Username
promptUsername mun =
  liftIO $
    case mun of
      Nothing -> promptUntil "username" parseUsername
      Just un -> pure un

promptPassword :: Maybe Text -> CliM Text
promptPassword mp =
  liftIO $
    case mp of
      Nothing -> promptSecret "password"
      Just pw -> pure pw

promptUntil :: Text -> (Text -> Maybe a) -> IO a
promptUntil p func = do
  s <- prompt p
  case func s of
    Nothing -> promptUntil p func
    Just a -> pure a

prompt :: Text -> IO Text
prompt = promptRaw True . (<> " > ")

promptSecret :: Text -> IO Text
promptSecret = promptRaw False . (<> " > ")

promptRaw :: Bool -> Text -> IO Text
promptRaw b p = do
  T.putStr p
  hFlush stdout
  pass <- withEcho b T.getLine
  unless b $ putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
