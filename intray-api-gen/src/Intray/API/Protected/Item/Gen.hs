{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected.Item.Gen where

import Import
import Intray.API.Protected
import Intray.Data.Gen ()

instance GenValid TypedItem where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenValid a) => GenValid (AddedItem a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid a) => GenValid (ItemInfo a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
