{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Sync
  ( autoSyncStore,
    tryToSyncStore,
    anyUnsyncedWarning,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Set as S
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Intray.Cli.DB
import Intray.Cli.Env
import Intray.Cli.OptParse
import Intray.Cli.Session
import Intray.Cli.Sqlite
import Intray.Client (ItemUUID, SyncResponse (..), Token, clientPostSync)
import Servant.Client

autoSyncStore :: CliM ()
autoSyncStore = do
  syncStrategy <- asks envSyncStrategy
  case syncStrategy of
    NeverSync -> pure ()
    AlwaysSync -> do
      mClientEnv <- asks envClientEnv
      case mClientEnv of
        Nothing -> pure ()
        Just clientEnv -> do
          mToken <- loadToken
          forM_ mToken $ \token ->
            actuallySync clientEnv token

tryToSyncStore :: CliM ()
tryToSyncStore = do
  mClientEnv <- asks envClientEnv
  case mClientEnv of
    Nothing -> logErrorN "No server configured."
    Just clientEnv -> withToken $ actuallySync clientEnv

actuallySync :: ClientEnv -> Token -> CliM ()
actuallySync clientEnv token = do
  syncRequest <- makeSyncRequest
  mShownItemUuid <- do
    mSi <- getShownItem
    runDB $
      fmap (join . join) $
        forM mSi $
          fmap (fmap clientItemServerIdentifier)
            . get
  errOrSyncResponse <- liftIO $ runClientM (clientPostSync token syncRequest) clientEnv
  case errOrSyncResponse of
    Left err -> logErrorN $ T.pack $ unlines ["Failed to sync:", show err]
    Right syncResponse -> do
      -- If the shown item was deleted then we have to clear it because
      -- otherwise it will refer to a row that won't exist anymore when the
      -- sync response is merged.
      let shownItemWasDeleted = case mShownItemUuid :: Maybe ItemUUID of
            Nothing -> False
            Just i ->
              S.member i (syncResponseServerDeleted syncResponse)
                || S.member i (syncResponseClientDeleted syncResponse)
      when shownItemWasDeleted clearShownItem
      mergeSyncResponse syncResponse
      logInfoN $ T.pack $ "Sync succesful, stats:\n" <> showSyncStats syncResponse
      runDB anyUnsyncedWarning

showSyncStats :: SyncResponse ci si a -> String
showSyncStats SyncResponse {..} =
  unlines
    [ unwords [show $ length syncResponseServerAdded, "added   remotely"],
      unwords [show $ length syncResponseServerDeleted, "deleted remotely"],
      unwords [show $ length syncResponseClientAdded, "added   locally"],
      unwords [show $ length syncResponseClientDeleted, "deleted locally"]
    ]

anyUnsyncedWarning :: (MonadIO m, MonadLogger m) => SqlPersistT m ()
anyUnsyncedWarning = do
  mUnsynced <- selectFirst [ClientItemServerIdentifier ==. Nothing] []
  case mUnsynced of
    Nothing -> pure ()
    Just _ ->
      logWarnN $
        T.pack $
          unlines
            [ "Not all added items were synchronized in the most recent synchronisation.",
              "This may have occurred if you have not subscribed with your sync server.",
              "If that is the case, please navigate to your sync server's web interface to subscribe."
            ]
