{-# LANGUAGE OverloadedStrings #-}
module Zbot.Tests.Onion (onionTests) where

import Zbot.Core.Bot.Mock
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Service.Describe
import Zbot.Service.Describe.Onion
import Zbot.TestCase

import Test.Tasty

import Data.Time


services = registerService_ $ describe [describeOnion]

onionTests = testGroup "Onion Tests" [
    mockBotTestCase
      "Onion 1"
      services
      (shout "https://sports.theonion.com/kobe-bryant-claims-he-would-ve-won-12-rings-if-shaq-s-d-1837709955")
      [replyOutput "#channel" "Breaking News: Kobe Bryant Claims He Would\8217ve Won 12 Rings If Shaq\8217s Deadbeat Father Was Around To Instill Stronger Work Ethic"],

    mockBotTestCase
      "Onion 2"
      services
      (shout "https://ogn.theonion.com/fascinating-lore-nintendo-revealed-that-the-reason-mar-1835703486")
      [replyOutput "#channel" "Breaking News: Fascinating Lore: Nintendo Revealed That The Reason Mario Always Comes Back To Life After He Dies Is Because Both Heaven And Hell Reject His Soul"],

    mockBotTestCase
      "Onion 3"
      services
      (shout "https://www.theonion.com/the-onion-s-guide-to-red-dead-redemption-2-1830437412")
      [replyOutput "#channel" "Breaking News: The Onion\8217s Guide To \8216Red Dead Redemption 2\8217"]
  ]
  where shout x = [Shout "#channel" "nick" x]
