{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()
import qualified Data.Text as T
import Intray.API
import System.IO.Unsafe
import Test.QuickCheck

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

instance GenValid AccountInfo where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ChangePassphrase where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid PaidStatus where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Username where
  genValid = do
    username <- parseUsername <$> textGen
    case username of
      Just name -> pure name
      Nothing -> genValid
    where
      textGen =
        T.pack
          <$> ((:) <$> charGen <*> ((:) <$> charGen <*> ((:) <$> charGen <*> genListOf charGen)))
      charGen = choose ('\NUL', '\255') `suchThat` validUsernameChar
  shrinkValid = shrinkValidStructurally

instance GenValid AccessKeySecret where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid AdminStats where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ActiveUsers where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

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
