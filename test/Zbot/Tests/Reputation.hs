{-# LANGUAGE OverloadedStrings #-}
module Zbot.Tests.Reputation (reputationTests) where

import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Service.Reputation
import Zbot.TestCase

import Test.Tasty

services = registerService_ reputation

reputationTests = testGroup "Reputations Tests" [
    zbotTestCase
      "Initial State Test"
      services
      [Shout "#channel" "nick" "!rep"]
      []

  , zbotTestCase
      "+1 nick"
      services
      [
        Shout "#channel" "nick" "+1 foo"
      , Shout "#channel" "nick" "!rep"
      ]
      ["[->IRC] (BestEffort) PRIVMSG #channel :foo has 1 rep\r\n"]

  , zbotTestCase
      "-1 nick"
      services
      [
        Shout "#channel" "nick" "-1 foo"
      , Shout "#channel" "nick" "!rep"
      ]
      ["[->IRC] (BestEffort) PRIVMSG #channel :foo has -1 rep\r\n"]

  , zbotTestCase
      "nick++"
      services
      [
        Shout "#channel" "nick" "foo++"
      , Shout "#channel" "nick" "!rep"
      ]
      ["[->IRC] (BestEffort) PRIVMSG #channel :foo has 1 rep\r\n"]

  , zbotTestCase
      "++nick"
      services
      [
        Shout "#channel" "nick" "++foo"
      , Shout "#channel" "nick" "!rep"
      ]
      ["[->IRC] (BestEffort) PRIVMSG #channel :foo has 1 rep\r\n"]
  ]
