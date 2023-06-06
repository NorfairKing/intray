{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Add
  ( getAddR,
    postAddR,
  )
where

import qualified Data.Text as T
import Import
import Intray.API.Image
import Intray.Client
import Intray.Web.Server.Foundation
import qualified Network.HTTP.Types as Http
import Yesod

getAddR :: Handler Html
getAddR =
  withLogin $ \_ -> do
    alreadyExpired
    req <- getRequest
    let tokenKey = defaultCsrfParamName
    let token =
          case reqToken req of
            Nothing -> mempty
            Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]
    withNavBar $(widgetFile "add")

newItemTextForm :: FormInput Handler Textarea
newItemTextForm = ireq textareaField "contents"

newItemImageForm :: FormInput Handler FileInfo
newItemImageForm = ireq fileField "image"

postAddR :: Handler Html
postAddR =
  withLogin $ \t -> do
    tfr <- runInputPostResult newItemTextForm
    let goOn _ = do
          ifr <- runInputPostResult newItemImageForm
          case ifr of
            FormSuccess fi -> do
              rawImageData <- fileSourceByteString fi
              itemData <-
                case downsizeImage rawImageData of
                  Left err -> invalidArgs ["Failed to resize the image: ", T.pack err]
                  Right bs -> pure bs
              itemType <-
                case parseImageType (fileContentType fi) of
                  Left err -> invalidArgs [T.pack err]
                  Right typ -> pure $ ImageItem typ
              pure TypedItem {..}
            _ -> redirect AddR -- There's nothing we can do with the errors, so just redirect so the user can continue.
    ti <-
      case tfr of
        FormSuccess ta -> pure $ textTypedItem $ unTextarea ta
        FormFailure ts -> goOn ts
        FormMissing -> goOn []
    errOrRes <- runClient $ clientPostAddItem t ti
    case errOrRes of
      Left err ->
        handleStandardServantErrs err $ \resp ->
          case responseStatusCode resp of
            c
              | c == Http.unauthorized401 -> addNegativeMessage "You are not allowed to add items."
              | c == Http.paymentRequired402 ->
                  addNegativeMessage
                    "You have reached the limit of the free plan, subscribe to be able to add more items. Click 'Account' to get started."
              | otherwise -> sendResponseStatus Http.status500 $ show resp
      Right _ -> pure ()
    redirect AddR
