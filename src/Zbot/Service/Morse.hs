{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Morse (
    morse
) where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Command
import Zbot.Extras.UnitService

import Control.Monad.Trans.Class (lift)
import Data.Char (toLower)

import qualified Data.Map as M
import qualified Data.Text as T


-- | A service that will translate text into morse code.
morse :: Bot m => Service m ()
morse = (unitService  "Zbot.Service.Morse" (onCommand "!morse" handleCommand)) {
      helpSpec = Just HelpSpec {
              helpAliases = ["!morse"]
          ,   helpMessage = ["usage: !morse <input>"]
          }
    }

handleCommand :: Bot m => Reply m -> T.Text -> MonadService () m ()
handleCommand reply arg = lift $ reply $ encodeMorse arg

encodeMorse :: T.Text -> T.Text
encodeMorse = T.concatMap encode . T.intersperse ' '
    where
        encode char = case M.lookup (toLower char) morseCode of
            Just code -> code
            Nothing   -> T.singleton char

morseCode :: M.Map Char T.Text
morseCode =
  M.fromList
  [('a', ".-")
  ,('b', "-...")
  ,('c', "-.-.")
  ,('d', "-..")
  ,('e', ".")
  ,('f', "..-.")
  ,('g', "--.")
  ,('h', "....")
  ,('i', "..")
  ,('j', ".---")
  ,('k', "-.-")
  ,('l', ".-..")
  ,('m', "--")
  ,('n', "-.")
  ,('o', "---")
  ,('p', ".--.")
  ,('q', "--.-")
  ,('r', ".-.")
  ,('s', "...")
  ,('t', "-")
  ,('u', "..-")
  ,('v', "...-")
  ,('w', ".--")
  ,('x', "-..-")
  ,('y', "-.--")
  ,('z', "--..")
  ,('=', "-...-")
  ,('?', "..--..")
  ,('/', "-..-.")
  ,(',', "--..--")
  ,('.', ".-.-.-")
  ,(':', "---...")
  ,('\'', ".----.")
  ,('-', "-....-")
  ,('(', "-.--.")
  ,(')', "-.--.-")
  ,('0', "-----")
  ,('1', ".----")
  ,('2', "..---")
  ,('3', "...--")
  ,('4', "....-")
  ,('5', ".....")
  ,('6', "-....")
  ,('7', "--...")
  ,('8', "---..")
  ,('9', "----.")
  ,('@', ".--.-.")]
