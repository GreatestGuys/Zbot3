{-# LANGUAGE OverloadedStrings #-}
module Zbot.Tests.Reputation (reputationTests) where

import Zbot.Core.Bot.Mock
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Service.Reputation
import Zbot.TestCase

import Test.Tasty


services = registerService_ reputation

reputationTests = testGroup "Reputations Tests" [
    mockBotTestCase
      "Initial State Test"
      services
      [Shout "#channel" "nick" "!rep"]
      []

  , mockBotTestCase
      "+1 nick"
      services
      [
        Shout "#channel" "nick" "+1 foo"
      , Shout "#channel" "nick" "!rep"
      ]
      [replyOutput "#channel" "foo has 1 rep"]

  , mockBotTestCase
      "-1 nick"
      services
      [
        Shout "#channel" "nick" "-1 foo"
      , Shout "#channel" "nick" "!rep"
      ]
      [replyOutput "#channel" "foo has -1 rep"]

  , mockBotTestCase
      "nick++"
      services
      [
        Shout "#channel" "nick" "foo++"
      , Shout "#channel" "nick" "!rep"
      ]
      [replyOutput "#channel" "foo has 1 rep"]

  , mockBotTestCase
      "++nick"
      services
      [
        Shout "#channel" "nick" "++foo"
      , Shout "#channel" "nick" "!rep"
      ]
      [replyOutput "#channel" "foo has 1 rep"]
  ]
