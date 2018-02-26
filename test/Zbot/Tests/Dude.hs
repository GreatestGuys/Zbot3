{-# LANGUAGE OverloadedStrings #-}
module Zbot.Tests.Dude (dudeTests) where

import Zbot.Core.Bot.Mock
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Service.Dude
import Zbot.TestCase

import Test.Tasty

import Data.Time


services = registerService_ dude

dudeTests = testGroup "Dude Tests" [
    mockBotTestCase
      "- responds to o/ with \\o"
      services
      [ Time (ts !! 0), Shout "#channel" "nick" "o/"
      , Time (ts !! 5)]
      [replyOutput "#channel" "\\o"]

  , mockBotTestCase
      "- responds to \\o with o/"
      services
      [ Time (ts !! 0), Shout "#channel" "nick" "\\o"
      , Time (ts !! 5)]
      [replyOutput "#channel" "o/"]

  , mockBotTestCase
      "- no response if hanging for under 5 minutes"
      services
      [ Time (ts !! 0), Shout "#channel" "nick" "o/"
      , Time (ts !! 4)]
      []

  , mockBotTestCase
      "- only responds to dudes still left hanging"
      services
      [ Time (ts !! 0), Shout "#channel" "nick" "o/", Shout "#channel" "nick2" "\\o"
      , Time (ts !! 5)]
      []

  , mockBotTestCase
      "- hangings dudes are matched FIFO"
      services
      [ Time (ts !! 0), Shout "#channel" "nick" "o/"
      , Time (ts !! 3), Shout "#channel" "nick2" "o/"
      , Time (ts !! 4), Shout "#channel" "nick3" "\\o"
      , Time (ts !! 5)]
      []

  , mockBotTestCase
      "- dude service respects channels"
      services
      [ Time (ts !! 0), Shout "#channel" "nick" "o/", Shout "#channel2" "nick2" "\\o"
      , Time (ts !! 5)]
      [ replyOutput "#channel" "\\o"
      , replyOutput "#channel2" "o/"]

  , mockBotTestCase
      "- doesn't leave big headed doods (BHDs) hanging"
      services
      [ Time (ts !! 0), Shout "#channel" "nick" "O/"
      , Time (ts !! 5), Shout "#channel" "nick" "\\O"
      , Time (ts !! 10)]
      [ replyOutput "#channel" "\\o"
      , replyOutput "#channel" "o/"]

  , mockBotTestCase
      "- dudes are expired after they've seen 5 non-dude lines"
      services
      [ Time (ts !! 0), Shout "#channel" "nick" "o/"
      , Shout "#channel" "nick1" "1"
      , Shout "#channel" "nick2" "2"
      , Shout "#channel" "nick3" "3"
      , Shout "#channel" "nick4" "4"
      , Shout "#channel" "nick4" "o/"
      , Shout "#channel" "nick5" "5"
      , Time (ts !! 1)]
      [replyOutput "#channel" "\\o"]

  , mockBotTestCase
      "- dudes don't expire if lines spoken are under 5 and minutes hung are under 5"
      services
      [ Time (ts !! 0), Shout "#channel" "nick" "o/"
      , Shout "#channel" "nick1" "1"
      , Shout "#channel" "nick2" "2"
      , Shout "#channel" "nick3" "3"
      , Shout "#channel" "nick4" "4"
      , Shout "#channel" "nick4" "o/"
      , Time (ts !! 4)]
      []
  ]
    where
        ts = iterate (addUTCTime 60)
           $ UTCTime (ModifiedJulianDay 0) 0
