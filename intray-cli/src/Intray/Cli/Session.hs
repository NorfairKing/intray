module Intray.Cli.Session
  ( withToken,
    loadToken,
    clearSession,
    loadSession,
    saveSession,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Intray.Cli.Env
import Path
import Path.IO
import Servant.Auth.Client
import System.Exit
import Web.Cookie

withToken :: (Token -> CliM a) -> CliM a
withToken func = do
  mToken <- loadToken
  case mToken of
    Nothing -> liftIO $ die "Please log in first."
    Just token -> func token

loadToken :: CliM (Maybe Token)
loadToken = do
  mCookie <- loadSession
  pure $ Token . setCookieValue <$> mCookie

clearSession :: CliM ()
clearSession = do
  p <- sessionPath
  liftIO $ ignoringAbsence $ removeFile p

loadSession :: CliM (Maybe SetCookie)
loadSession = do
  p <- sessionPath
  mContents <- liftIO $ forgivingAbsence $ SB.readFile $ toFilePath p
  pure $ parseSetCookie <$> mContents

saveSession :: SetCookie -> CliM ()
saveSession setCookie = do
  p <- sessionPath
  liftIO $ do
    ensureDir $ parent p
    LB.writeFile (toFilePath p) $ SBB.toLazyByteString $ renderSetCookie setCookie

sessionPath :: CliM (Path Abs File)
sessionPath = do
  d <- asks envCacheDir
  resolveFile d "session.cookie"
