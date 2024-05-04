module Intray.Cli (intrayCli, dispatch) where

import Intray.Cli.Commands.Add
import Intray.Cli.Commands.Done
import Intray.Cli.Commands.Login
import Intray.Cli.Commands.Logout
import Intray.Cli.Commands.Register
import Intray.Cli.Commands.Review
import Intray.Cli.Commands.Show
import Intray.Cli.Commands.Size
import Intray.Cli.Commands.Sync
import Intray.Cli.Env
import Intray.Cli.OptParse
import System.FileLock

intrayCli :: IO ()
intrayCli = getInstructions >>= dispatch

dispatch :: Instructions -> IO ()
dispatch (Instructions d settings) = do
  let run = runCliM settings
  case d of
    DispatchRegister rs -> run Shared Shared $ register rs
    DispatchLogin ls -> run Shared Shared $ login ls
    DispatchAddItem t -> run Exclusive Exclusive $ addItem t
    DispatchShowItem -> run Exclusive Exclusive showItem
    DispatchDoneItem -> run Exclusive Exclusive doneItem
    DispatchSize -> run Shared Exclusive size
    DispatchLogout -> run Shared Shared logout
    DispatchSync -> run Exclusive Exclusive sync
    DispatchReview -> review settings
