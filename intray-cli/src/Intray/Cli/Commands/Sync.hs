module Intray.Cli.Commands.Sync (sync) where

import Intray.Cli.Env
import Intray.Cli.Sync

sync :: CliM ()
sync = tryToSyncStore
