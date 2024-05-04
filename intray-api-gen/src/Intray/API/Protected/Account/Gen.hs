{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected.Account.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()
import qualified Data.Text as T
import Intray.API
import Intray.API.Admin.Gen ()
import Intray.API.Username
import Test.QuickCheck

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
