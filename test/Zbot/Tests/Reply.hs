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
      (registerService_ $ replyService reply)
      [Shout "#channel" "nick" "message"]
      [replyOutput "#channel" "message"],

    mockBotTestCase
      "Reply Direct on PM"
      (registerService_ $ replyService reply)
      [Whisper "nick" "message"]
      [replyOutput "nick" "message"],

    mockBotTestCase
      "Reply ForceWhisper on channel"
      (registerService_ $ replyService whisperBack)
      [Shout "#channel" "nick" "message"]
      [replyOutput "nick" "message"],

    mockBotTestCase
      "Reply ForceWhisper on PM"
      (registerService_ $ replyService whisperBack)
      [Whisper "nick" "message"]
      [replyOutput "nick" "message"],

    mockBotTestCase
      "Reply ShoutOrDrop on channel"
      (registerService_ $ replyService shoutBackOrDrop)
      [Shout "#channel" "nick" "message"]
      [replyOutput "#channel" "message"],

    mockBotTestCase
      "Reply ForceWhisper on PM"
      (registerService_ $ replyService shoutBackOrDrop)
      [Whisper "nick" "message"]
      []
  ]
  where
      replyService :: Bot m => (MessageContext m -> Reply m) -> Service m ()
      replyService mode = unitService "test service"
                        $ onMessage (\ctx msg -> lift $ mode ctx msg)
