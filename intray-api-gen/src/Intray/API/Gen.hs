{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Gen
  ( module Intray.API.Gen,
    module Intray.API.Admin.Gen,
    module Intray.API.Protected.Gen,
  )
where

import Data.GenValidity
import Intray.API
import Intray.API.Admin.Gen ()
import Intray.API.Protected.Gen ()
import System.IO.Unsafe

instance GenValid Registration where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid LoginForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Pricing where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid HashedPassword where
  genValid = do
    t <- genValid
    case unsafePerformIO $ passwordHash t of
      Nothing -> error "unable to hash password during generation, for some reason"
      Just hp -> pure hp
  shrinkValid _ = [] -- Doesn't help anyway

instance GenValid Permission where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AccessKeySecret where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
