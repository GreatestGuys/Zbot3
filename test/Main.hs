{-# LANGUAGE OverloadedStrings #-}
module Main where

import Zbot.Tests.Dude
import Zbot.Tests.Reputation
import Zbot.Tests.Roll

import Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "ZBot Tests" [
    dudeTests
  , reputationTests
  , rollTests
  ]
