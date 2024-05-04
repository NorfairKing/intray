module Intray.Cli.Commands.Size (size) where

import Control.Monad.IO.Class
import Intray.Cli.Env
import Intray.Cli.Sqlite
import Intray.Cli.Sync

size :: CliM ()
size = do
  autoSyncStore
  c <- getStoreSize
  liftIO $ print c
