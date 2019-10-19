{-# LANGUAGE OverloadedStrings #-}
module Main where

import Zbot.Tests.Dude
import Zbot.Tests.Onion
import Zbot.Tests.Replace
import Zbot.Tests.Reputation
import Zbot.Tests.Roll
import Zbot.Tests.Typo
import Zbot.Tests.YouTube

import Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "ZBot Tests" [
    dudeTests
  , onionTests
  , replaceTests
  , reputationTests
  , rollTests
  , typoTests
  , youTubeTests
  ]
