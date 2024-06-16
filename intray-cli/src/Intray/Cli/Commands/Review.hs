{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Commands.Review (review) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Time
import Intray.Cli.Commands.Done
import Intray.Cli.Env
import Intray.Cli.OptParse
import Intray.Cli.Prompt
import Intray.Cli.Sqlite
import Intray.Cli.Store
import Intray.Cli.Sync
import System.FileLock

review :: Settings -> IO ()
review settings = go (pure ())
  where
    go :: CliM () -> IO ()
    go beforeFunc = do
      mClientItemAndStoreSize <- runCliM settings Exclusive Exclusive $ do
        beforeFunc
        autoSyncStore
        mShownItem <- produceShownItem
        forM mShownItem $ \clientItem -> do
          s <- getStoreSize
          pure (clientItem, s)
      case mClientItemAndStoreSize of
        Nothing -> putStrLn "Done. Yay!"
        Just (clientItem, s) -> do
          now <- liftIO getCurrentTime
          putStrLn $ unwords [show s, "items remaining"]
          prettyShowItemAndWait (setAutoOpen settings) (setCacheDir settings) now clientItem
          res <- liftIO $ prompt "done [y/N]"
          let cont = go doneItem
              stop = pure ()
          case res of
            "y" -> cont
            "Y" -> cont
            "n" -> stop
            "N" -> stop
            "" -> stop
            _ -> go (pure ())
