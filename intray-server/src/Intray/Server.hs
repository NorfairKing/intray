{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server
  ( runIntrayServer,
    makeIntrayServer,
    intrayAppContext,
  )
where

import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist.Sqlite
import Import
import Intray.API
import Intray.Server.OptParse.Types
import Intray.Server.Serve (intrayServer)
import Intray.Server.SigningKey
import Intray.Server.Types
import Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Server.Generic

runIntrayServer :: Settings -> IO ()
runIntrayServer Settings {..} =
  runStderrLoggingT
    $ filterLogger (\_ ll -> ll >= setLogLevel)
    $ withSqlitePoolInfo setConnectionInfo 1
    $ \pool -> do
      runResourceT $ flip runSqlPool pool $ runMigration serverAutoMigration
      signingKey <- liftIO $ loadSigningKey setSigningKeyFile
      let jwtCfg = defaultJWTSettings signingKey
      let cookieCfg = defaultCookieSettings
      logFunc <- askLoggerIO
      let intrayEnv =
            IntrayServerEnv
              { envLogFunc = logFunc,
                envHost = setHost,
                envConnectionPool = pool,
                envCookieSettings = cookieCfg,
                envJWTSettings = jwtCfg,
                envAdmins = setAdmins,
                envFreeloaders = setFreeloaders,
                envMonetisation = setMonetisationSettings
              }
      let runServer = Warp.run setPort $ intrayApp intrayEnv
      liftIO runServer

intrayApp :: IntrayServerEnv -> Wai.Application
intrayApp se = addPolicy . serveWithContext intrayAPI (intrayAppContext se) $ makeIntrayServer se
  where
    addPolicy = cors (const $ Just policy)
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type"],
          corsMethods = ["GET", "POST", "HEAD", "DELETE"]
        }

{-# ANN makeIntrayServer ("NOCOVER" :: String) #-}
makeIntrayServer :: IntrayServerEnv -> Server IntrayAPI
makeIntrayServer cfg =
  hoistServerWithContext
    intrayAPI
    (Proxy :: Proxy IntrayContext)
    (\func -> runLoggingT (runReaderT func cfg) (envLogFunc cfg))
    (genericServerT intrayServer)

intrayAppContext :: IntrayServerEnv -> Context IntrayContext
intrayAppContext se@IntrayServerEnv {..} = envCookieSettings :. envJWTSettings :. accessKeyEnv se :. EmptyContext

accessKeyEnv :: IntrayServerEnv -> AccessKeyEnv
accessKeyEnv IntrayServerEnv {..} = AccessKeyEnv {accessKeyEnvAuthenticate = go}
  where
    go :: Username -> Text -> IO (AuthResult AuthCookie)
    go un ak = flip runSqlPool envConnectionPool $ do
      mUser <- getBy $ UniqueUsername un
      case mUser of
        Nothing -> pure NoSuchUser
        Just (Entity _ user) -> do
          aks <- selectList [AccessKeyUser ==. userIdentifier user] [Asc AccessKeyCreatedTimestamp]
          let mli =
                flip map aks $ \(Entity _ AccessKey {..}) -> do
                  submittedKey <- parseAccessKeySecretText ak
                  if validatePassword accessKeyHashedKey (accessKeySecretText submittedKey)
                    then Right accessKeyPermissions
                    else Left "invalid password"
          pure $ case msum mli of
            Left _ -> BadPassword
            Right perms -> Authenticated $ AuthCookie {authCookieUserUUID = userIdentifier user, authCookiePermissions = perms}

type IntrayContext = '[CookieSettings, JWTSettings, AccessKeyEnv]
