{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Commands.Add (addItem) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Time
import qualified Database.Persist.Sql as DB
import Intray.API
import Intray.Cli.DB
import Intray.Cli.Env
import Intray.Cli.OptParse
import Intray.Cli.Sync

addItem :: AddSettings -> CliM ()
addItem addSettings = do
  mItemContents <- liftIO $ getItemContents addSettings
  forM_ mItemContents $ \contents -> do
    addItemLocally contents
    autoSyncStore

getItemContents :: AddSettings -> IO (Maybe Text)
getItemContents AddSettings {..} =
  case (addSetReadStdin, addSetContents) of
    (False, []) -> pure Nothing
    (True, []) -> Just <$> liftIO T.getContents
    (False, cts) -> pure $ Just $ T.unwords cts
    (True, cts) ->
      Just <$> do
        cts' <- liftIO T.getContents
        pure $ T.intercalate "\n" [T.unwords cts, cts']

addItemLocally :: Text -> CliM ()
addItemLocally contents = runDB $ do
  now <- liftIO getCurrentTime
  DB.insert_
    ClientItem
      { clientItemType = TextItem,
        clientItemContents = TE.encodeUtf8 contents,
        clientItemCreated = now,
        clientItemAccessKeyName = Nothing,
        clientItemServerIdentifier = Nothing,
        clientItemDeleted = False
      }
