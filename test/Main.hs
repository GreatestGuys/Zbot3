{-# LANGUAGE OverloadedStrings #-}
module Main where

import Zbot.Tests.Dude
import Zbot.Tests.Grep
import Zbot.Tests.Help
import Zbot.Tests.Onion
import Zbot.Tests.Replace
import Zbot.Tests.Reply
import Zbot.Tests.Reputation
import Zbot.Tests.Roll
import Zbot.Tests.Typo
import Zbot.Tests.YouTube

import Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "ZBot Tests" [
    dudeTests
  , grepTests
  , helpTests
  , onionTests
  , replaceTests
  , replyTests
  , reputationTests
  , rollTests
  , typoTests
  , youTubeTests
  ]
