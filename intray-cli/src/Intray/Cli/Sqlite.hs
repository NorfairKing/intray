{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Sqlite
  ( clearShownItem,
    getShownItem,
    produceShownItem,
    deleteClientItem,
    getStoreSize,
    makeSyncRequest,
    mergeSyncResponse,
  )
where

import Control.Monad
import Data.Int
import qualified Data.Map as M
import Data.Mergeless (SyncRequest (..), SyncResponse (..))
import Data.Mergeless.Persistent as Mergeless
import Database.Persist as DB
import Database.Persist.Sql as DB
import Intray.API
import Intray.Cli.DB
import Intray.Cli.Env

setShownItem :: ClientItemId -> CliM ()
setShownItem clientItemId =
  void $
    runDB $
      DB.upsertBy
        (UniqueShownItem clientItemId)
        (ShownItem {shownItemItem = clientItemId})
        [ShownItemItem =. clientItemId]

clearShownItem :: CliM ()
clearShownItem = runDB $ DB.deleteWhere ([] :: [Filter ShownItem])

getShownItem :: CliM (Maybe ClientItemId)
getShownItem = runDB $ fmap (shownItemItem . entityVal) <$> DB.selectFirst [] []

produceShownItem :: CliM (Maybe (Entity ClientItem))
produceShownItem = do
  mShownItemId <- getShownItem
  mShownItem <- fmap join $
    forM mShownItemId $
      \shownItem -> do
        clientItem <- runDB $ DB.get shownItem
        pure (Entity shownItem <$> clientItem)
  case mShownItem of
    Just clientItem -> pure (Just clientItem)
    Nothing -> do
      mClientItem <- runDB $ DB.selectFirst [ClientItemDeleted ==. False] []
      forM mClientItem $ \clientItemEntity@(Entity clientItemId _) -> do
        setShownItem clientItemId
        pure clientItemEntity

deleteClientItem :: ClientItemId -> CliM ()
deleteClientItem cid = runDB $ do
  mClientItem <- get cid
  forM_ mClientItem $ \ClientItem {..} ->
    case clientItemServerIdentifier of
      -- The item is local-only, delete it directly
      Nothing -> DB.delete cid
      -- The item is on the server as well, only mark it as deleted.
      Just _ -> DB.update cid [ClientItemDeleted =. True]

getStoreSize :: CliM Int
getStoreSize = runDB $ DB.count [ClientItemDeleted ==. False]

makeSyncRequest :: CliM (SyncRequest Int64 ItemUUID (AddedItem TypedItem))
makeSyncRequest =
  runDB $
    (\sr -> sr {syncRequestAdded = M.mapKeys DB.fromSqlKey (syncRequestAdded sr)})
      <$> Mergeless.clientMakeSyncRequestQuery from ClientItemServerIdentifier ClientItemDeleted
  where
    from :: ClientItem -> AddedItem TypedItem
    from ClientItem {..} =
      AddedItem
        { addedItemCreated = clientItemCreated,
          addedItemContents =
            TypedItem
              { itemType = clientItemType,
                itemData = clientItemContents
              },
          addedItemAccessKeyName = clientItemAccessKeyName
        }

mergeSyncResponse :: SyncResponse Int64 ItemUUID (AddedItem TypedItem) -> CliM ()
mergeSyncResponse =
  runDB
    . Mergeless.clientMergeSyncResponseQuery to ClientItemServerIdentifier ClientItemDeleted
    . (\sr -> sr {syncResponseClientAdded = M.mapKeys DB.toSqlKey (syncResponseClientAdded sr)})
  where
    to :: ItemUUID -> AddedItem TypedItem -> ClientItem
    to serverId AddedItem {..} =
      let TypedItem {..} = addedItemContents
       in ClientItem
            { clientItemType = itemType,
              clientItemContents = itemData,
              clientItemCreated = addedItemCreated,
              clientItemAccessKeyName = addedItemAccessKeyName,
              clientItemServerIdentifier = Just serverId,
              clientItemDeleted = False
            }
