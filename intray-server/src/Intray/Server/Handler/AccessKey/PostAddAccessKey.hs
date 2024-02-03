{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.AccessKey.PostAddAccessKey
  ( servePostAddAccessKey,
  )
where

import qualified Data.Set as S
import Data.Time
import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

servePostAddAccessKey :: AuthCookie -> AddAccessKey -> IntrayHandler AccessKeyCreated
servePostAddAccessKey AuthCookie {..} AddAccessKey {..} = do
  let perms = authCookiePermissions `S.intersection` addAccessKeyPermissions
  unless (perms == addAccessKeyPermissions) $ throwAll err401
  uuid <- liftIO nextRandomUUID
  now <- liftIO getCurrentTime
  secret <- liftIO generateRandomAccessKeySecret
  mhp <- liftIO $ passwordHash $ accessKeySecretText secret
  case mhp of
    Nothing -> throwAll err500 {errBody = "Unable to hash secret key."}
    Just hp -> do
      runDB
        $ insert_
          AccessKey
            { accessKeyIdentifier = uuid,
              accessKeyUser = authCookieUserUUID,
              accessKeyName = addAccessKeyName,
              accessKeyHashedKey = hp,
              accessKeyCreatedTimestamp = now,
              accessKeyPermissions = perms
            }
      pure
        AccessKeyCreated
          { accessKeyCreatedCreatedTimestamp = now,
            accessKeyCreatedKey = secret,
            accessKeyCreatedUUID = uuid
          }
