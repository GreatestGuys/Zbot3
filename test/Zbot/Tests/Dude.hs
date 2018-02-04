{-# LANGUAGE OverloadedStrings #-}
module Zbot.Tests.Dude (dudeTests) where

import Zbot.Core.Bot.Mock
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Service.Dude
import Zbot.TestCase

import Test.Tasty


services = registerService_ dude

dudeTests = testGroup "Dude Tests" [
    mockBotTestCase
      "Positive Dude Test"
      services
      [Shout "#channel" "nick" "o/"]
      [replyOutput "#channel" "\\o"]

  , mockBotTestCase
      "Negative Dude Test"
      services
      [Shout "#channel" "nick" "o"]
      []
  ]
