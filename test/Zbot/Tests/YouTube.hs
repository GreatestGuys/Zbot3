{-# LANGUAGE OverloadedStrings #-}
module Zbot.Tests.YouTube (youTubeTests) where

import Zbot.Core.Bot.Mock
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Service.Describe
import Zbot.Service.Describe.YouTube
import Zbot.TestCase

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Text.Regex.TDFA
import qualified Text.Regex.Base.RegexLike as RE


services = registerService_ (describe [describeYouTube])

youTubeTests = testGroup "YouTube Tests" [
    youTubeTestCase
        "https://www.youtube.com/watch?v=xCD3hg6OEQw"
        "Slavoj Zizek on KUNG FU PANDA \\[length: 00:02:30, views: [0-9,]+\\]",

    youTubeTestCase
        "https://www.youtube.com/watch?v=ImFA7F571TA"
        "Everybody's Gone .* Soundtrack \\[length: 01:05:58, views: [0-9,]+\\]",

    youTubeTestCase
        "https://youtu.be/I25UeVXrEHQ"
        "Richard Stallman .* His Foot \\[length: 00:02:22, views: [0-9,]+\\]",

    youTubeTestCase
        "foo bar https://www.youtube.com/watch?feature=youtu.be&v=XT9IF4CMQDI"
        "Playing a 7/11 .* 11 seconds \\[length: 00:07:16, views: [0-9,]+\\]"
 ]
 where
    youTubeTestCase url pattern =
        mockBotTestCaseWithAssert
          (T.unpack url)
          services
          [Shout "#channel" "nick" url]
          (\actual -> do
            [message] <- return $ map outputMessage actual
            RE.matchTest (re pattern) message @?
                (concat [T.unpack message, " does not match ", pattern]))

re :: String -> Text.Regex.TDFA.Regex
re = Text.Regex.TDFA.makeRegex
