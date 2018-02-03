{-# LANGUAGE OverloadedStrings #-}
module Zbot.Tests.Dude (dudeTests) where

import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Service.Dude
import Zbot.TestCase

import Test.Tasty

services = registerService_ dude

dudeTests = testGroup "Dude Tests" [
    zbotTestCase
      "Positive Dude Test"
      services
      [Shout "#channel" "nick" "o/"]
      ["[->IRC] (BestEffort) PRIVMSG #channel :\\o\r\n"]

  , zbotTestCase
      "Negative Dude Test"
      services
      [Shout "#channel" "nick" "o"]
      []
  ]
