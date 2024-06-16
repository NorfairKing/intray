{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Intray.Cli.OptParse
  ( Instructions (..),
    getInstructions,
    Settings (..),
    SyncStrategy (..),
    AutoOpen (..),
    Dispatch (..),
    RegisterSettings (..),
    LoginSettings (..),
    AddSettings (..),
  )
where

import Autodocodec.Yaml
import Control.Monad.Logger
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Env
import Intray.API.Username
import Intray.Cli.OptParse.Types
import OptEnvConf
import Options.Applicative
import qualified Options.Applicative.Help as OptParse
import Path
import Path.IO
import Paths_intray_cli (version)
import Servant.Client
import qualified System.Environment as System

getInstructions :: IO Instructions
getInstructions = runSettingsParser version
