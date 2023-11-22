{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.Handler.AuthRSpec where

import Intray.Data (parseUsername)
import Intray.Web.Server.Foundation
import Intray.Web.Server.TestUtils
import Network.HTTP.Types
import Test.Syd.Yesod
import TestImport

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec =
  intrayWebServerSpec $ do
    describe "RegisterR" $ do
      yit "gets a 200" $ do
        get $ AuthR registerR
        statusIs 200
      yit "registers an example account correctly" $ do
        registerFlow "example" "example"
        statusIs 303
        loc <- getLocation
        liftIO $ loc `shouldBe` Right AddR
        void followRedirect
        statusIs 200
      yit "fails to register and shows an error if an account with the same username exists" $ do
        registerFlow "example" "example"
        registerFlow "example" "example2"
        statusIs 303
        loc <- getLocation
        liftIO $ loc `shouldBe` Right (AuthR registerR)
        void followRedirect
        statusIs 200
        bodyContains "exists"
      yit "fails to register and shows an error if the username is not valid" $ do
        let un = "example with a space"
        liftIO $ parseUsername un `shouldBe` Nothing
        registerFlow un "example"
        statusIs 400
    describe "ChangePasswordR" $ do
      yit "gets a 200 when not logged in" $ do
        get $ AuthR changePasswordTargetR
        statusIs 200
      yit "gets a 200 when logged in" $
        withExampleAccountAndLogin_ $ do
          get $ AuthR changePasswordTargetR
          statusIs 200
      yit "can change a password using a succesful form fill" $
        withExampleAccountAndLogin $ \_ pw -> do
          get $ AuthR changePasswordTargetR
          statusIs 200
          let newPassword = "new password"
          request $ do
            setMethod methodPost
            setUrl $ AuthR changePasswordTargetR
            addTokenFromCookie
            addPostParam "old" pw
            addPostParam "new1" newPassword
            addPostParam "new2" newPassword
          statusIs 303
          locationShouldBe AccountR
          void followRedirect
          statusIs 200

      yit "cannot change a password using a incorrect form fill" $
        withExampleAccountAndLogin $ \_ pw -> do
          get $ AuthR changePasswordTargetR
          statusIs 200
          request $ do
            setMethod methodPost
            setUrl $ AuthR changePasswordTargetR
            addTokenFromCookie
            addPostParam "old" pw
            addPostParam "new1" "new password"
            addPostParam "new2" "different new password"
          statusIs 400

      yit "cannot change a password using a incorrect old password" $
        withExampleAccountAndLogin $ \_ _ -> do
          get $ AuthR changePasswordTargetR
          statusIs 200
          let newPassword = "new password"
          request $ do
            setMethod methodPost
            setUrl $ AuthR changePasswordTargetR
            addTokenFromCookie
            addPostParam "old" "other old password"
            addPostParam "new1" newPassword
            addPostParam "new2" newPassword
          statusIs 400

registerFlow :: Text -> Text -> YesodExample App ()
registerFlow exampleUsername examplePassphrase = do
  get $ AuthR registerR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR registerR
    addTokenFromCookie
    addPostParam "username" exampleUsername
    addPostParam "passphrase" examplePassphrase
    addPostParam "passphrase-confirm" examplePassphrase
