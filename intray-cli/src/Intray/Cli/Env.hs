{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Env where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Database.Persist.Sql
import Database.Persist.Sqlite
import Intray.Cli.DB
import Intray.Cli.OptParse.Types
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Path
import Path.IO
import Servant.Client
import System.FileLock

type CliM = ReaderT Env (LoggingT IO)

data Env = Env
  { envDataDir :: !(Path Abs Dir),
    envCacheDir :: !(Path Abs Dir),
    envAutoOpen :: !AutoOpen,
    envSyncStrategy :: !SyncStrategy,
    envConnectionPool :: !ConnectionPool,
    envClientEnv :: !(Maybe ClientEnv)
  }

runCliM :: Settings -> SharedExclusive -> SharedExclusive -> CliM a -> IO a
runCliM Settings {..} minimalExclusivity maximalExclusivity func = do
  dbPath <- resolveFile setDataDir "intray.sqlite3"
  dbLockPath <- resolveFile setDataDir "intray.sqlite3.lock"

  ensureDir (parent dbLockPath)
  let actualExclusivity = case setSyncStrategy of
        NeverSync -> minimalExclusivity
        AlwaysSync -> maximalExclusivity
  withFileLock (fromAbsFile dbLockPath) actualExclusivity $ \_ -> do
    liftIO $ ensureDir (parent dbPath)
    runStderrLoggingT
      . filterLogger (\_ ll -> ll >= setLogLevel)
      $ withSqlitePoolInfo (mkSqliteConnectionInfo $ T.pack $ fromAbsFile dbPath) 1
      $ \pool -> do
        flip runSqlPool pool $ do
          _ <- runMigrationQuiet clientAutoMigration
          pure ()

        mClientEnv <- forM setBaseUrl $ \burl -> do
          man <- liftIO $ HTTP.newManager tlsManagerSettings
          pure $ mkClientEnv man burl

        runReaderT
          func
          Env
            { envDataDir = setDataDir,
              envCacheDir = setCacheDir,
              envAutoOpen = setAutoOpen,
              envSyncStrategy = setSyncStrategy,
              envConnectionPool = pool,
              envClientEnv = mClientEnv
            }

runDB :: SqlPersistT (LoggingT IO) a -> CliM a
runDB query = do
  pool <- asks envConnectionPool
  logFunc <- askLoggerIO
  liftIO $ runLoggingT (runSqlPool (retryOnBusy query) pool) logFunc
