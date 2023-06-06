{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Account
  ( getAccountR,
    postAccountDeleteR,
  )
where

import Data.Time
import Import
import Intray.Client
import Intray.Web.Server.Foundation
import Intray.Web.Server.Time
import Yesod
import Yesod.Auth

getAccountR :: Handler Html
getAccountR =
  withLogin $ \t -> do
    mai <- runClientOrDisallow $ clientGetAccountInfo t
    mPricing <- runClientOrErr clientGetPricing
    accountInfoWidget <- accountInfoSegment mai mPricing
    token <- genToken
    withNavBar $(widgetFile "account")

accountInfoSegment :: Maybe AccountInfo -> Maybe Pricing -> Handler Widget
accountInfoSegment Nothing _ =
  pure
    [whamlet|
        <div .is-negative .message>
            You are not authorised to view account info.
            |]
accountInfoSegment (Just AccountInfo {..}) mp = do
  now <- liftIO getCurrentTime
  let subbedWidget =
        case accountInfoStatus of
          HasNotPaid _ -> [whamlet|Not subscribed|]
          HasPaid subbed -> [whamlet|Subscribed until ^{makeTimestampWidget now subbed}|]
          NoPaymentNecessary -> [whamlet|No payment necessary|]
      createdWidget = makeTimestampWidget now accountInfoCreatedTimestamp
  pure $
    mconcat
      [ [whamlet|
          <h3 .has-text-weight-bold>
            Info
          <p> Username: #{usernameText accountInfoUsername}
          <p> Created: ^{createdWidget}
          $maybe _ <- mp
            <p>
              Status: ^{subbedWidget}
        |],
        case accountInfoStatus of
          HasNotPaid _ -> maybe mempty pricingStripeForm mp
          _ -> mempty -- Already subscribed or no payment necessary
      ]

pricingStripeForm :: Pricing -> Widget
pricingStripeForm p =
  [whamlet|
          <h2 .title .is-4> Subscribe

          <p>
            <ul>
              <li>
                #{pricingPrice p} per year
              <li>
                Unlimited items
              <li>
                Full API access
          <p>
            <a .button .is-primary href=@{CheckoutR}>
              Checkout
    |]

adminSegment :: Maybe AccountInfo -> Widget
adminSegment Nothing = mempty
adminSegment (Just AccountInfo {..})
  | accountInfoAdmin =
      [whamlet|
          <div .columns .is-centered>
            <div .column .is-half>
              <div .content>
                <h2 .title .is-4>
                  Admin
                <p>
                  This account is an administrator.
                <p>
                  <a .is-success .button href=@{AdminR AdminPanelR}>
                    The Admin Panel|]
  | otherwise = mempty

postAccountDeleteR :: Handler Html
postAccountDeleteR =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientDeleteAccount t
    clearCreds False
    redirect HomeR
