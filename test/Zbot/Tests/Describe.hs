{-# LANGUAGE OverloadedStrings #-}
module Zbot.Tests.Describe (describeTests) where

import Zbot.Core.Bot
import Zbot.Core.Bot.Mock
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Service.Describe
import Zbot.TestCase

import Test.Tasty
import Data.Time


echoDescriber :: Describer
echoDescriber link = Just $ return $ return link

services = registerService_ $ describe [echoDescriber]

describeTests = testGroup "Describe Tests" [
    mockBotTestCase
      "A message without a link"
      services
      [Shout "#channel" "nick" "message"]
      [],

    mockBotTestCase
      "A message that is just an http:// link"
      services
      [Shout "#channel" "nick" "http://foobar.com"]
      [replyOutput "#channel" "http://foobar.com"],

    mockBotTestCase
      "A message that is just an https:// link"
      services
      [Shout "#channel" "nick" "https://foobar.com"]
      [replyOutput "#channel" "https://foobar.com"],

    mockBotTestCase
      "A message with a link infix"
      services
      [Shout "#channel" "nick" "message with https://foobar.com inside it"]
      [replyOutput "#channel" "https://foobar.com"],

    mockBotTestCase
      "A shouted command with a link"
      services
      [Shout "#channel" "nick" "!summary https://foobar.com"]
      [],

    mockBotTestCase
      "A whispered command with a link"
      services
      [Whisper "nick" "!summary https://foobar.com"]
      []
  ]
