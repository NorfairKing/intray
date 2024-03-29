{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Web.Server.Static where

import Intray.Web.Server.Constants
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote

mkEmbeddedStatic
  development
  "myStatic"
  [ embedFile "static/gtd_flowchart.jpg",
    embedFile "static/tom-sydney-kerckhove_ideas.svg",
    embedFile "static/tom-sydney-kerckhove_thinking.svg",
    embedFile "static/tom-sydney-kerckhove_flowchart-gtd.svg",
    embedFile "static/tom-sydney-kerckhove_faq.svg",
    embedFile "static/favicon.ico",
    embedFile "static/tom-sydney-kerckhove_github-icon.svg",
    embedFile "static/tom-sydney-kerckhove_linkedin-icon.svg",
    embedFile "static/tom-sydney-kerckhove_tiktok-icon.svg",
    embedFile "static/tom-sydney-kerckhove_twitter-icon.svg",
    embedFile "static/tom-sydney-kerckhove_youtube-icon.svg",
    embedFile "static/tom-sydney-kerckhove_logo-intray.svg",
    embedRemoteFile
      "static/semantic/themes/default/assets/fonts/icons.ttf"
      "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.ttf",
    embedRemoteFile
      "static/semantic/themes/default/assets/fonts/icons.woff"
      "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff",
    embedRemoteFile
      "static/semantic/themes/default/assets/fonts/icons.woff2"
      "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff2",
    embedRemoteFile "static/jquery.min.js" "https://code.jquery.com/jquery-3.1.1.min.js",
    embedRemoteFile
      "static/bulma/bulma.min.css"
      "https://cdn.jsdelivr.net/npm/bulma@0.9.2/css/bulma.min.css",
    embedRemoteFile
      "static/bulma/bulma-tooltip.min.css"
      "https://cdn.jsdelivr.net/npm/bulma-tooltip@3.0.2/dist/css/bulma-tooltip.min.css"
  ]
