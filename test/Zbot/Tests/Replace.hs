{-# LANGUAGE OverloadedStrings #-}
module Zbot.Tests.Replace (replaceTests) where

import Zbot.Core.Bot.Mock
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Service.History
import Zbot.Service.Replace
import Zbot.TestCase

import Test.Tasty

import Data.Time


services = fmap replace (history >>= registerService) >>= registerService_

replaceTests = testGroup "Replace Tests" [
    mockBotTestCase
      "- simple replace"
      services
      [ Shout "#channel" "nick" "hello world"
      , Shout "#channel" "nick" "s/hello/goodbye"]
      [replyOutput "#channel" "nick: goodbye world"]

  , mockBotTestCase
      "- replace someone else"
      services
      [ Shout "#channel" "red" "hello world"
      , Shout "#channel" "blue" "s/hello/goodbye"]
      [replyOutput "#channel" "red: goodbye world"]

  , mockBotTestCase
      "- handles Time events"
      services
      [ Shout "#channel" "nick" "hello world"
      , Time (head ts)
      , Shout "#channel" "nick" "s/hello/goodbye"]
      [replyOutput "#channel" "nick: goodbye world"]
  ]
    where
        ts = iterate (addUTCTime 60)
           $ UTCTime (ModifiedJulianDay 0) 0
