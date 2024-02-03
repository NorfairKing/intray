{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.GetItemUUIDs
  ( serveGetItemUUIDs,
  )
where

import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Types

serveGetItemUUIDs :: AuthCookie -> IntrayHandler [ItemUUID]
serveGetItemUUIDs AuthCookie {..} =
  fmap (fmap $ intrayItemIdentifier . entityVal)
    $ runDB
    $ selectList [IntrayItemUserId ==. authCookieUserUUID] [Asc IntrayItemCreated]
