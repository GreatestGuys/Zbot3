{-# LANGUAGE OverloadedStrings #-}
module Zbot.Tests.Help (helpTests) where

import Zbot.Core.Bot
import Zbot.Core.Bot.Mock
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Message
import Zbot.Extras.UnitService
import Zbot.Service.Help
import Zbot.TestCase

import Control.Monad.Trans.Class (lift)
import Test.Tasty
import Data.Time


services = do
    registerService_ help
    registerService_ $ (unitService "test" (\_ -> return ())) {
        helpSpec = Just HelpSpec {
                helpAliases = ["!test", "!alt"]
            ,   helpMessage = ["1", "2", "3"]
            }
      }

helpTests = testGroup "Help Tests" [
    mockBotTestCase
      "Test list services"
      services
      [Shout "#channel" "nick" "!help"]
      [
        replyOutput "#channel" "Check your DM's ;)"
      , replyOutput "nick" "usage: !help [command]"
      , replyOutput "nick" " "
      , replyOutput "nick" "   The !help command is used to display usage and other"
      , replyOutput "nick" "general information about a component. When no argument is"
      , replyOutput "nick" "given, this message will be displayed."
      , replyOutput "nick" " "
      , replyOutput "nick" "Services with help pages:"
      , replyOutput "nick" " "
      , replyOutput "nick" "    Zbot.Service.Help: !help, !halp"
      , replyOutput "nick" "    test: !test, !alt"
      ],

    mockBotTestCase
      "Unknown command from channel"
      services
      [Shout "#channel" "nick" "!help asdf"]
      [replyOutput "#channel" "There is no such help page."],

    mockBotTestCase
      "Unknown command from whisper"
      services
      [Whisper "nick" "!help asdf"]
      [replyOutput "nick" "There is no such help page."],

    mockBotTestCase
      "By first alias"
      services
      [Shout "#channel" "nick" "!help !test"]
      [
        replyOutput "#channel" "Check your DM's ;)"
      , replyOutput "nick" "1"
      , replyOutput "nick" "2"
      , replyOutput "nick" "3"
      ],

    mockBotTestCase
      "By second alias"
      services
      [Shout "#channel" "nick" "!help !test"]
      [
        replyOutput "#channel" "Check your DM's ;)"
      , replyOutput "nick" "1"
      , replyOutput "nick" "2"
      , replyOutput "nick" "3"
      ]
  ]
