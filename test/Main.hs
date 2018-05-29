{-# LANGUAGE OverloadedStrings #-}
module Main where

import Zbot.Tests.Dude
import Zbot.Tests.Replace
import Zbot.Tests.Reputation

import Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "ZBot Tests" [
    dudeTests,
    replaceTests,
    reputationTests
  ]
