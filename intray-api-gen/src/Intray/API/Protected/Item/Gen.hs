{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected.Item.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()
import Intray.API.ItemType
import Intray.API.Protected

instance GenValid TypedItem where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenValid a) => GenValid (AddedItem a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid a) => GenValid (ItemInfo a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ItemType where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ImageType where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
