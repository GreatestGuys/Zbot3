{-# LANGUAGE OverloadedStrings #-}
module Zbot.TestCase (
  mockBotTestCase
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
mockBotTestCase description botInit events expected = testCase description $
  withSystemTempDirectory "zbot-test" $ \tempDir -> do
    -- Drop the initial handshake since it is common among all tests.
    actual <- drop 3 <$> evalMockBot tempDir botInit events
    actual @?= expected

-- | Creates an Output value that corresponds to Zbot sending a message to a
-- channel.
replyOutput :: T.Text -> T.Text -> Output
replyOutput channel message =
    Output BestEffort (Message Nothing "PRIVMSG" [channel] (Just message))
