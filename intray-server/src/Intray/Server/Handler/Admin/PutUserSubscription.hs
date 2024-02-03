module Intray.Server.Handler.Admin.PutUserSubscription
  ( serveAdminPutUserSubscription,
  )
where

import Data.Time
import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant

serveAdminPutUserSubscription :: AuthCookie -> Username -> UTCTime -> IntrayHandler NoContent
serveAdminPutUserSubscription _ username end = do
  mUserEntity <- runDB $ getBy (UniqueUsername username)
  case mUserEntity of
    Nothing -> throwError err404
    Just (Entity _ user) ->
      let uuid = userIdentifier user
       in runDB
            $ void
            $ upsertBy
              (UniqueSubscriptionUser uuid)
              ( Subscription
                  { subscriptionUser = uuid,
                    subscriptionEnd = end
                  }
              )
              [ SubscriptionEnd =. end
              ]
  pure NoContent
