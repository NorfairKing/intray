{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.Handler.AddRSpec where

import Control.Monad.Reader
import qualified Data.Set as S
import Intray.Client
import Intray.Server.TestUtils as TestUtils
import Intray.Web.Server.Foundation
import Intray.Web.Server.TestUtils
import Network.HTTP.Types
import Test.Syd.Yesod
import TestImport

spec :: Spec
spec =
  intrayWebServerSpec $
    describe "AddR" $ do
      it "gets a 200 for an example user" $
        withExampleAccount_ $ do
          get AddR
          statusIs 200

      it "can add an item with an access token" $ do
        (un, secret) <- withExampleAccount $ \un pw -> do
          -- Add the access token
          man <- asks $ appHttpManager . yesodClientSite
          burl <- asks $ appAPIBaseUrl . yesodClientSite
          let cenv = mkClientEnv man burl
          let name = "access-key-name-dummy"
          secret <- liftIO $ do
            token <- TestUtils.login cenv un pw
            runClientOrError cenv $ do
              accessKeyCreatedKey
                <$> clientPostAddAccessKey
                  token
                  AddAccessKey
                    { addAccessKeyName = name,
                      addAccessKeyPermissions = S.singleton PermitAdd
                    }
          pure (un, secret)

        -- Open the add page with username and access key
        request $ do
          setMethod methodGet
          setUrl AddR
          addGetParam "username" $ usernameText un
          addGetParam "access-key" $ accessKeySecretText secret
        statusIs 200

        let contents = "dummy-contents"
        request $ do
          setMethod methodPost
          setUrl AddR
          addToken
          addPostParam "contents" contents
        statusIs 303
        locationShouldBe AddR

      let roundtripTest contents =
            it ("roundtrips " <> show contents) $
              withExampleAccount $ \un pw -> do
                get AddR
                statusIs 200
                request $ do
                  setMethod methodPost
                  setUrl AddR
                  addToken
                  addPostParam "contents" contents
                statusIs 303
                loc <- getLocation
                liftIO $ loc `shouldBe` Right AddR
                man <- asks $ appHttpManager . yesodClientSite
                burl <- asks $ appAPIBaseUrl . yesodClientSite
                let cenv = mkClientEnv man burl
                liftIO $ do
                  token <- TestUtils.login cenv un pw
                  runClientOrError cenv $ do
                    uuids <- clientGetItemUUIDs token
                    case uuids of
                      [uuid] -> do
                        ii <- clientGetItem token uuid
                        liftIO $ itemInfoContents ii `shouldBe` textTypedItem contents
                      _ -> liftIO $ expectationFailure "expected exactly one item"
      roundtripTest "hello"
      roundtripTest "hello\n"
      roundtripTest "hello\nworld"
      roundtripTest "hello\nworld\n"
      roundtripTest "hello\n\nworld"
      roundtripTest "hello\n\nworld\n"
      roundtripTest "hello\r\nworld"
      roundtripTest "hello\r\nworld\n"
