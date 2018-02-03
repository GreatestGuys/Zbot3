module Zbot.TestCase where

import Zbot.Core.Irc
import Zbot.Core.Bot.Mock

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Temp

import qualified Data.Text as T

zbotTestCase :: String -> MockBot () -> [Event] -> [T.Text] -> TestTree
zbotTestCase description botInit events expected = testCase description $ do
  withSystemTempDirectory "zbot-test" $ \tempDir -> do
    -- Drop the initial handshake since it is common among all tests.
    actual <- drop 3 <$> evalMockBot tempDir botInit events
    actual @?= expected
