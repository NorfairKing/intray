{-# LANGUAGE LambdaCase #-}

module Intray.Cli.Client where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Intray.Cli.Env
import Servant.Client
import System.Exit

runSingleClientOrErr :: ClientM a -> CliM (Maybe a)
runSingleClientOrErr func = do
  mErrOrRes <- runSingleClient func
  forM mErrOrRes $ \case
    Left err -> liftIO $ die $ unlines ["Error while contacting the intray server:", show err]
    Right r -> pure r

runSingleClient :: ClientM a -> CliM (Maybe (Either ClientError a))
runSingleClient func = do
  mCenv <- asks envClientEnv
  forM mCenv $ \cenv ->
    liftIO $ runClientM func cenv
