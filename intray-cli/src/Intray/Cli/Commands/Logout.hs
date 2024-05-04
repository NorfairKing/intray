module Intray.Cli.Commands.Logout (logout) where

import Intray.Cli.Env
import Intray.Cli.Session

logout :: CliM ()
logout = clearSession
