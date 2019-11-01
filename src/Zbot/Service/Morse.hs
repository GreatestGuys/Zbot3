{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Morse (
    morse
) where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Message
import Zbot.Extras.UnitService

import Control.Monad.Trans.Class (lift)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T


-- | A service that will translate text into morse code.
morse :: Bot m => Service m ()
morse = (unitService  "Zbot.Service.Morse" $ onMessage handler) {
      helpSpec = Just HelpSpec {
              helpAliases = ["!morse", "!unmorse"]
          ,   helpMessage = ["usage: !morse <input>"
                          ,  "       !unmorse <input>"]
          }
    }

handler :: Bot m => MessageContext m -> T.Text -> MonadService () m ()
handler ctx msg
    | "!morse"   == cmd = lift . reply ctx $ encodeMorse args
    | "!unmorse" == cmd = lift . reply ctx $ decodeMorse args
    | otherwise         = return ()
    where
        (cmd,args) = T.breakOn " " msg

encodeMorse :: T.Text -> T.Text
encodeMorse = T.concatMap encode . T.intersperse ' '
    where
        encode char = fromMaybe (T.singleton char)
                                (M.lookup (toLower char) toMorseCode)

decodeMorse :: T.Text -> T.Text
decodeMorse = T.concat . map decode . L.intercalate [" "] . map chars . words
      where
          words = T.splitOn "   "

          chars = T.splitOn " "

          decode char = case M.lookup char fromMorseCode of
              Just letter -> T.singleton letter
              Nothing     -> char

fromMorseCode :: M.Map T.Text Char
fromMorseCode = M.foldrWithKey (flip M.insert) M.empty toMorseCode

toMorseCode :: M.Map Char T.Text
toMorseCode =
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
