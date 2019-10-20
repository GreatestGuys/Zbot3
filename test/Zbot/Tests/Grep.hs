{-# LANGUAGE OverloadedStrings #-}
module Zbot.Tests.Grep (grepTests) where

import Zbot.Core.Bot
import Zbot.Core.Bot.Mock
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Color
import Zbot.Extras.Message
import Zbot.Extras.UnitService
import Zbot.Service.Grep
import Zbot.Service.History
import Zbot.TestCase

import Control.Monad.Trans.Class (lift)
import Test.Tasty
import Data.Time

services = fmap grep (history >>= registerService) >>= registerService_

grepTests = testGroup "Grep Tests" [
    mockBotTestCase
      "Match found when requested from channel"
      services
      [
        Shout "#channel" "nick" "foo"
      , Shout "#channel" "nick" "bar"
      , Shout "#channel" "nick" "!grep -T o"
      ]
      [replyOutput "#channel" $ colorize ["#channel nick> f", fg Red "o", "o"]],

    mockBotTestCase
      "Match not found when requested from channel"
      services
      [
        Shout "#channel" "nick" "foo"
      , Shout "#channel" "nick" "bar"
      , Shout "#channel" "nick" "!grep -T x"
      ]
      [replyOutput "#channel" "No matches found."],

    mockBotTestCase
      "Match found when requested from PM"
      services
      [
        Shout "#channel" "nick" "foo"
      , Shout "#channel" "nick" "bar"
      , Whisper "nick" "!grep -C #channel -T o"
      ]
      [replyOutput "nick" $ colorize ["#channel nick> f", fg Red "o", "o"]],

    mockBotTestCase
      "Hide command by default"
      services
      [
        Shout "#channel" "nick" "!foo"
      , Shout "#channel" "nick" "!grep -T foo"
      ]
      [replyOutput "#channel" "No matches found."],

    mockBotTestCase
      "-S allows commands to be found"
      services
      [
        Shout "#channel" "nick" "!foo"
      , Shout "#channel" "nick" "!grep -T -S foo"
      ]
      [replyOutput "#channel" $ colorize ["#channel nick> !", fg Red "foo"]],

    mockBotTestCase
      "Returns the most recent match"
      services
      [
        Shout "#channel" "a" "foo"
      , Shout "#channel" "b" "foo"
      , Shout "#channel" "nick" "!grep -T foo"
      ]
      [replyOutput "#channel" $ colorize ["#channel b> ", fg Red "foo"]],

    mockBotTestCase
      "-n allows matches to be filtered by nick"
      services
      [
        Shout "#channel" "a" "foo"
      , Shout "#channel" "b" "foo"
      , Shout "#channel" "nick" "!grep -T -n a foo"
      ]
      [replyOutput "#channel" $ colorize ["#channel a> ", fg Red "foo"]],

    mockBotTestCase
      "Only search current channel by default"
      services
      [
        Shout "#other" "nick" "foo"
      , Shout "#channel" "nick" "!grep -T foo"
      ]
      [replyOutput "#channel" "No matches found."],

    mockBotTestCase
      "-C allows other channels to be searched"
      services
      [
        Shout "#other" "nick" "foo"
      , Shout "#channel" "nick" "!grep -T -C #other foo"
      ]
      [replyOutput "#channel" $ colorize ["#other nick> ", fg Red "foo"]],

    mockBotTestCase
      "-m allows multiple matches to be returned"
      services
      [
        Shout "#channel" "nick" "foo1"
      , Shout "#channel" "nick" "foo2"
      , Shout "#channel" "nick" "foo3"
      , Shout "#channel" "nick" "foo4"
      , Shout "#channel" "nick" "!grep -T -m 3 foo"
      ]
      [
        replyOutput "#channel" $ colorize ["#channel nick> ", fg Red "foo", "2"]
      , replyOutput "#channel" $ colorize [fg Cyan "----"]
      , replyOutput "#channel" $ colorize ["#channel nick> ", fg Red "foo", "3"]
      , replyOutput "#channel" $ colorize [fg Cyan "----"]
      , replyOutput "#channel" $ colorize ["#channel nick> ", fg Red "foo", "4"]
      ],

    mockBotTestCase
      "-C displays multiple lines of context surrounding the match"
      services
      [
        Shout "#channel" "nick" "1"
      , Shout "#channel" "nick" "2"
      , Shout "#channel" "nick" "3"
      , Shout "#channel" "nick" "foo"
      , Shout "#channel" "nick" "4"
      , Shout "#channel" "nick" "5"
      , Shout "#channel" "nick" "6"
      , Shout "#channel" "nick" "!grep -T -c 2 foo"
      ]
      [
        replyOutput "#channel" $ colorize ["#channel nick> 2"]
      , replyOutput "#channel" $ colorize ["#channel nick> 3"]
      , replyOutput "#channel" $ colorize ["#channel nick> ", fg Red "foo"]
      , replyOutput "#channel" $ colorize ["#channel nick> 4"]
      , replyOutput "#channel" $ colorize ["#channel nick> 5"]
      ]
  ]
