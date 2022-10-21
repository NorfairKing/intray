{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Admin.GetStats
  ( serveAdminGetStats,
  )
where

import Data.Time
import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Stripe
import Intray.Server.Handler.Utils
import Intray.Server.Types

serveAdminGetStats :: AuthCookie -> IntrayHandler AdminStats
serveAdminGetStats _ = do
  adminStatsNbAccounts <- fmap fromIntegral $ runDB $ count ([] :: [Filter User])
  adminStatsNbItems <- fmap fromIntegral $ runDB $ count ([] :: [Filter IntrayItem])
  now <- liftIO getCurrentTime
  let day :: NominalDiffTime
      day = 86400
  let activeUsers time =
        fmap fromIntegral $ runDB $ count [UserLastLogin >=. Just (addUTCTime (- time) now)]
  activeUsersDaily <- activeUsers day
  activeUsersWeekly <- activeUsers $ 7 * day
  activeUsersMonthly <- activeUsers $ 30 * day
  activeUsersYearly <- activeUsers $ 365 * day
  let adminStatsActiveUsers = ActiveUsers {..}
  adminStatsSubscribedUsers <- do
    us <- runDB $ selectList [] []
    fmap (fromIntegral . length . catMaybes) $
      forM us $ \(Entity _ u) -> do
        ps <- getUserPaidStatus (userIdentifier u)
        pure $ case ps of
          HasNotPaid _ -> Nothing
          HasPaid t -> Just t
          NoPaymentNecessary -> Nothing
  pure AdminStats {..}
