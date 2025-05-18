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

intrayCli :: IO ()
intrayCli = getInstructions >>= dispatch

dispatch :: Instructions -> IO ()
dispatch (Instructions d settings) = do
  let run = runCliM settings
  case d of
    DispatchRegister rs -> run $ register rs
    DispatchLogin ls -> run $ login ls
    DispatchAddItem t -> run $ addItem t
    DispatchShowItem -> run showItem
    DispatchDoneItem -> run doneItem
    DispatchSize -> run size
    DispatchLogout -> run logout
    DispatchSync -> run sync
    DispatchReview -> review settings
