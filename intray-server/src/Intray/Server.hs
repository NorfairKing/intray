{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server
  ( runIntrayServer,
    makeIntrayServer,
    intrayAppContext,
  )
where

import Control.Concurrent.Async
import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Data.Cache
import Database.Persist.Sqlite
import Import
import Intray.API
import Intray.Server.Looper (LoopersSettings (..), runIntrayServerLoopers)
import Intray.Server.OptParse.Types
import Intray.Server.Serve (intrayServer)
import Intray.Server.SigningKey
import Intray.Server.Types
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Server.Generic

runIntrayServer :: ServeSettings -> IO ()
runIntrayServer ServeSettings {..} =
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= serveSetLogLevel) $
      withSqlitePoolInfo serveSetConnectionInfo 1 $
        \pool -> do
          runResourceT $ flip runSqlPool pool $ runMigration migrateAll
          signingKey <- liftIO $ loadSigningKey serveSetSigningKeyFile
          let jwtCfg = defaultJWTSettings signingKey
          let cookieCfg = defaultCookieSettings
          mMonetisationEnv <-
            forM serveSetMonetisationSettings $ \MonetisationSettings {..} -> do
              planCache <- liftIO $ newCache Nothing
              pure
                MonetisationEnv
                  { monetisationEnvStripeSettings = monetisationSetStripeSettings,
                    monetisationEnvMaxItemsFree = monetisationSetMaxItemsFree,
                    monetisationEnvPlanCache = planCache
                  }
          let intrayEnv =
                IntrayServerEnv
                  { envHost = serveSetHost,
                    envConnectionPool = pool,
                    envCookieSettings = cookieCfg,
                    envJWTSettings = jwtCfg,
                    envAdmins = serveSetAdmins,
                    envFreeloaders = serveSetFreeloaders,
                    envMonetisation = mMonetisationEnv
                  }
          let mLoopersSets =
                case serveSetMonetisationSettings of
                  Nothing -> Nothing
                  Just MonetisationSettings {..} ->
                    Just
                      LoopersSettings
                        { loopersSetLogLevel = serveSetLogLevel,
                          loopersSetConnectionPool = pool,
                          loopersSetStripeSettings = monetisationSetStripeSettings,
                          loopersSetStripeEventsFetcher = monetisationSetStripeEventsFetcher,
                          loopersSetStripeEventsRetrier = monetisationSetStripeEventsRetrier
                        }
          let runServer = Warp.run serveSetPort $ intrayApp intrayEnv
          case mLoopersSets of
            Nothing -> liftIO runServer
            Just ls -> do
              let runLoopers = runIntrayServerLoopers ls
              liftIO $ race_ runServer runLoopers

intrayApp :: IntrayServerEnv -> Wai.Application
intrayApp se = addPolicy . serveWithContext intrayAPI (intrayAppContext se) $ makeIntrayServer se
  where
    addPolicy = cors (const $ Just policy)
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type"],
          corsMethods = ["GET", "POST", "HEAD", "DELETE"]
        }

makeIntrayServer :: IntrayServerEnv -> Server IntrayAPI
makeIntrayServer cfg =
  hoistServerWithContext
    intrayAPI
    (Proxy :: Proxy IntrayContext)
    (`runReaderT` cfg)
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
                    then Just accessKeyPermissions
                    else Nothing
          pure $ case msum mli of
            Nothing -> BadPassword
            Just perms -> Authenticated $ AuthCookie {authCookieUserUUID = userIdentifier user, authCookiePermissions = perms}

type IntrayContext = '[CookieSettings, JWTSettings, AccessKeyEnv]
