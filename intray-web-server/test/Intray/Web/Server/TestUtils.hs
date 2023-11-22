{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.TestUtils
  ( intrayWebServerSpec,
    webServerSetupFunc,
    withExampleAccount,
    withExampleAccount_,
    withExampleAccountAndLogin,
    withExampleAccountAndLogin_,
    withAdminAccount,
    withAdminAccount_,
    withAdminAccountAndLogin,
    withAdminAccountAndLogin_,
  )
where

import Intray.Data
import Intray.Data.Gen ()
import qualified Intray.Server.TestUtils as API
import Intray.Web.Server.Application ()
import Intray.Web.Server.Foundation
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http
import Servant.Client (ClientEnv (..))
import Test.Syd.Yesod
import TestImport
import Yesod.Auth

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

intrayWebServerSpec :: YesodSpec App -> Spec
intrayWebServerSpec = API.withIntrayServer . yesodSpecWithSiteSetupFunc' webServerSetupFunc

webServerSetupFunc :: Http.Manager -> ClientEnv -> SetupFunc App
webServerSetupFunc man cenv =
  pure $
    App
      { appHttpManager = man,
        appStatic = myStatic,
        appTracking = Nothing,
        appVerification = Nothing,
        appAPIBaseUrl = baseUrl cenv
      }

loginTo :: Username -> Text -> YesodExample App ()
loginTo username passphrase = do
  get $ AuthR LoginR
  statusIs 200
  request $ do
    setMethod Http.methodPost
    setUrl $ AuthR loginFormPostTargetR
    addTokenFromCookie
    addPostParam "userkey" $ usernameText username
    addPostParam "passphrase" passphrase
  statusIs 303
  loc <- getLocation
  liftIO $ loc `shouldBe` Right AddR

withFreshAccount ::
  Username -> Text -> (Username -> Text -> YesodExample App a) -> YesodExample App a
withFreshAccount exampleUsername examplePassphrase func = do
  get $ AuthR registerR
  statusIs 200
  request $ do
    setMethod Http.methodPost
    setUrl $ AuthR registerR
    addTokenFromCookie
    addPostParam "username" $ usernameText exampleUsername
    addPostParam "passphrase" examplePassphrase
    addPostParam "passphrase-confirm" examplePassphrase
  statusIs 303
  loc <- getLocation
  liftIO $ loc `shouldBe` Right AddR
  func exampleUsername examplePassphrase

withExampleAccount :: (Username -> Text -> YesodExample App a) -> YesodExample App a
withExampleAccount = withFreshAccount (fromJust $ parseUsername "example") "pass"

withExampleAccountAndLogin :: (Username -> Text -> YesodExample App a) -> YesodExample App a
withExampleAccountAndLogin func =
  withExampleAccount $ \un p -> do
    loginTo un p
    func un p

withExampleAccount_ :: YesodExample App a -> YesodExample App a
withExampleAccount_ = withExampleAccount . const . const

withExampleAccountAndLogin_ :: YesodExample App a -> YesodExample App a
withExampleAccountAndLogin_ = withExampleAccountAndLogin . const . const

withAdminAccount :: (Username -> Text -> YesodExample App a) -> YesodExample App a
withAdminAccount = withFreshAccount (fromJust $ parseUsername "admin") "admin"

withAdminAccount_ :: YesodExample App a -> YesodExample App a
withAdminAccount_ = withAdminAccount . const . const

withAdminAccountAndLogin :: (Username -> Text -> YesodExample App a) -> YesodExample App a
withAdminAccountAndLogin func =
  withAdminAccount $ \un p -> do
    loginTo un p
    func un p

withAdminAccountAndLogin_ :: YesodExample App a -> YesodExample App a
withAdminAccountAndLogin_ = withAdminAccountAndLogin . const . const
