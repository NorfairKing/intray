{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Commands.Done (doneItem) where

import Control.Monad.Logger
import Intray.Cli.Env
import Intray.Cli.Sqlite
import Intray.Cli.Sync

doneItem :: CliM ()
doneItem = do
  mShownItem <- getShownItem
  case mShownItem of
    Nothing -> logWarnN "Are you sure?, it doesn't look like you showed an item yet."
    Just ci -> do
      clearShownItem
      deleteClientItem ci
      autoSyncStore
