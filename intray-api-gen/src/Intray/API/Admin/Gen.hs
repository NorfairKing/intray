{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Admin.Gen where

import Data.GenValidity
import Intray.API

instance GenValid AdminStats where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ActiveUsers where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
