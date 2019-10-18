{-# LANGUAGE OverloadedStrings #-}
module Zbot.TestCase (
  mockBotTestCase
, mockBotTestCaseWithAssert
, outputMessage
, replyOutput
) where

import Zbot.Core.Bot.Mock
import Zbot.Core.Irc
import Zbot.Core.Irc.Protocol

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Temp

import qualified Data.Text as T


-- | Creates a test case that executes a series of events in a mock bot and
-- asserts that the output matches that given.
mockBotTestCase :: String -> MockBot () -> [Event] -> [Output] -> TestTree
mockBotTestCase description botInit events expected =
    mockBotTestCaseWithAssert description botInit events (@?= expected)

-- | Creates a test case that executes a series of events in a mock bot and
-- performs a custom assertion on the output.
mockBotTestCaseWithAssert :: String -> MockBot () -> [Event] -> ([Output] -> Assertion) -> TestTree
mockBotTestCaseWithAssert description botInit events assertion = testCase description $
  withSystemTempDirectory "zbot-test" $ \tempDir -> do
    -- Drop the initial handshake since it is common among all tests.
    actual <- drop 3 <$> evalMockBot tempDir botInit events
    assertion actual

-- | Creates an Output value that corresponds to Zbot sending a message to a
-- channel.
replyOutput :: T.Text -> T.Text -> Output
replyOutput channel message =
    Output BestEffort (Message Nothing "PRIVMSG" [channel] (Just message))

-- | Returns the message component of an Output value.
outputMessage :: Output -> T.Text
outputMessage (Output _ (Message Nothing "PRIVMSG" [_] (Just msg))) = msg
