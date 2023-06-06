{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.AccessKeys
  ( getAccessKeysR,
    postAccessKeysR,
    postAccessKeyRevokeR,
  )
where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Import
import Intray.Client
import Intray.Web.Server.Foundation
import Intray.Web.Server.Time
import Yesod

getAccessKeysR :: Handler Html
getAccessKeysR =
  withLogin $ \t -> do
    maks <- runClientOrDisallow $ clientGetAccessKeys t
    aksw <- accessKeysWidget maks
    mperms <- runClientOrDisallow $ clientGetPermissions t
    nakf <- makeNewAccessKeyForm mperms
    withNavBar $(widgetFile "access-keys")

accessKeysWidget :: Maybe [AccessKeyInfo] -> Handler Widget
accessKeysWidget Nothing =
  pure
    [whamlet|
          <div .is-negative .message>
              You are not authorised to view access keys.|]
accessKeysWidget (Just accessKeys) = do
  accessKeysWidgets <- mapM accessKeyWidget accessKeys
  pure
    [whamlet|
          $forall akw <- accessKeysWidgets
            <div .box>
              ^{akw}|]

makeNewAccessKeyForm :: Maybe (Set Permission) -> Handler Widget
makeNewAccessKeyForm Nothing =
  pure
    [whamlet|
          <div .is-danger .message>
            You are not authorised to view this account's permissions.|]
makeNewAccessKeyForm (Just permissions) = do
  token <- genToken
  pure
    [whamlet|
          <div .box>
            <form .form
              method=post
              action=@{AccessKeysR}>
              <div .field>
                <label .label>
                  Name
                <div .control>
                  <input .input
                    type="text"
                    name="name"
                    required>

              $forall perm <- permissions
                <div .field>
                  <div .control>
                    <label .checkbox>
                      <input name=#{show perm} type=checkbox>
                      #{show perm}

              ^{token}
              <div .field>
                <div .control>
                  <button
                    .button
                    type=submit>
                    Add AccessKey|]

newAccessKeyForm :: Set Permission -> FormInput Handler AddAccessKey
newAccessKeyForm ps =
  AddAccessKey
    <$> ireq textField "name"
    <*> ( S.fromList . map fst . filter snd
            <$> traverse (\p -> (,) p <$> ireq checkBoxField (T.pack $ show p)) (S.toList ps)
        )

postAccessKeysR :: Handler Html
postAccessKeysR =
  withLogin $ \t -> do
    ps <- runClientOrErr $ clientGetPermissions t
    aac <- runInputPost $ newAccessKeyForm ps
    AccessKeyCreated {..} <- runClientOrErr $ clientPostAddAccessKey t aac
    timestampWidget <- makeTimestampWidgetNow accessKeyCreatedCreatedTimestamp
    withNavBar $(widgetFile "access-key-created")

postAccessKeyRevokeR :: AccessKeyUUID -> Handler Html
postAccessKeyRevokeR uuid =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientDeleteAccessKey t uuid
    redirect AccessKeysR

accessKeyWidget :: AccessKeyInfo -> Handler Widget
accessKeyWidget AccessKeyInfo {..} = do
  token <- genToken
  timestampWidget <- makeTimestampWidgetNow accessKeyInfoCreatedTimestamp
  pure $(widgetFile "access-key")
