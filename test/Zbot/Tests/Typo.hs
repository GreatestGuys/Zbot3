{-# LANGUAGE OverloadedStrings #-}
module Zbot.Tests.Typo (typoTests) where

import Zbot.Core.Bot.Mock
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Service.History
import Zbot.Service.Typo
import Zbot.TestCase

import Test.Tasty


services = fmap (typo 5) (history >>= registerService) >>= registerService_

typoTests = testGroup "Typo Tests" [
    mockBotTestCase
      "- basic typo"
      services
      [ Shout "#channel" "nick" "I love sonic the hedgehog"
      , Shout "#channel" "nick" "sanic*"]
      [replyOutput "#channel" "nick: I love sanic the hedgehog"]

  , mockBotTestCase
      "- replaces all occurrences of a typo"
      services
      [ Shout "#channel" "jesse" "all my occurences of occurences are misspelled"
      , Shout "#channel" "jesse" "occurrences*"]
      [replyOutput "#channel"
                   "jesse: all my occurrences of occurrences are misspelled"]

  , mockBotTestCase
      "- correction can be multiple words"
      services
      [ Shout "#channel" "nick" "I sure make alot of typos!"
      , Shout "#channel" "nick" "a lot*"]
      [replyOutput "#channel" "nick: I sure make a lot of typos!"]

  , mockBotTestCase
      "- you can only fix your own typos"
      services
      [ Shout "#channel" "jesse" "Brad--"
      , Shout "#channel" "zhenya" "--Brad*"]
      []

  , mockBotTestCase
      "- typos further than maxDistance aren't corrected"
      services
      [ Shout "#channel" "nick" "tnak"
      , Shout "#channel" "nick" "Rat King*"]
      []

  , mockBotTestCase
      "- don't correct messages like '*vomit*'"
      services
      [ Shout "#channel" "nick" "vmit"
      , Shout "#channel" "nick" "*vomit*"]
      []
  ]
