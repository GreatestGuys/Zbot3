{-# LANGUAGE OverloadedStrings #-}
module Zbot.Tests.Reply (replyTests) where

import Zbot.Core.Bot
import Zbot.Core.Bot.Mock
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Message
import Zbot.Extras.UnitService
import Zbot.TestCase

import Control.Monad.Trans.Class (lift)
import Test.Tasty
import Data.Time

replyTests = testGroup "OnMessage Reply Tests" [
    mockBotTestCase
      "Reply Direct on channel"
      (registerService_ $ replyService Direct)
      [Shout "#channel" "nick" "message"]
      [replyOutput "#channel" "message"],

    mockBotTestCase
      "Reply Direct on PM"
      (registerService_ $ replyService Direct)
      [Whisper "nick" "message"]
      [replyOutput "nick" "message"],

    mockBotTestCase
      "Reply ForceWhisper on channel"
      (registerService_ $ replyService ForceWhisper)
      [Shout "#channel" "nick" "message"]
      [replyOutput "nick" "message"],

    mockBotTestCase
      "Reply ForceWhisper on PM"
      (registerService_ $ replyService ForceWhisper)
      [Whisper "nick" "message"]
      [replyOutput "nick" "message"],

    mockBotTestCase
      "Reply ShoutOrDrop on channel"
      (registerService_ $ replyService ShoutOrDrop)
      [Shout "#channel" "nick" "message"]
      [replyOutput "#channel" "message"],

    mockBotTestCase
      "Reply ForceWhisper on PM"
      (registerService_ $ replyService ShoutOrDrop)
      [Whisper "nick" "message"]
      []
  ]
  where
      replyService :: Bot m => ReplyMode -> Service m ()
      replyService mode = unitService "test service"
                        $ onMessage (\reply msg -> lift $ reply mode msg)
