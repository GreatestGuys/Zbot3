{-# LANGUAGE OverloadedStrings #-}
module Zbot.Tests.Roll (rollTests) where

import Zbot.Service.Roll (exprParser, Expr(..))

import Data.Attoparsec.Text (parseOnly)

import Test.Tasty
import Test.Tasty.HUnit

rollTests = testGroup "Roll Tests" [
    exprTest
      "Parser takes constants"
      "42"
      (Const 42)
  , exprTest
      "Parser takes dice"
      "3d6"
      (Die 3 6)
  , exprTest
      "Parser can handle addition"
      "1d6 + 5d9 + 7"
      (Die 1 6 :+: Die 5 9 :+: Const 7)
  , exprTest
      "Parser can handle subtraction"
      "10 - 1d3"
      (Const 10 :-: Die 1 3)
  , exprTest
      "Negation is higher precedence than subtraction"
      "-1d20 - 2d6"
      (Negate (Die 1 20) :-: Die 2 6)
  , exprTest
      "Parser doesn't require spaces"
      "10-1d3"
      (Const 10 :-: Die 1 3)
  ]
  where
    exprTest desc exprString expr =
      testCase desc $
        (parseOnly exprParser exprString) @?=
        (Right $ expr)
