{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.IncompletionList
  ( getIncompletionListR,
  )
where

import Import
import Intray.Web.Server.Foundation
import Yesod

getIncompletionListR :: Handler Html
getIncompletionListR = do
  neverExpires
  withNavBar $ do
    setTitle "Incompletion List"
    $(widgetFile "incompletion-list")
