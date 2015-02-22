{-# LANGUAGE MultiWayIf #-}
module Zbot.Extras.Command (
    onCommand
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Extras.Message

import Control.Monad.State

import qualified Data.Text as T

onCommand :: Bot m
          => T.Text
          -> (Reply m -> [T.Text] -> StateT s m ())
          -> Event -> StateT s m ()
onCommand name handler = onMessage $ \reply msg -> if
    | isCommand name msg -> handler reply (toArgs name msg)
    | otherwise          -> return ()

isCommand :: T.Text -> T.Text -> Bool
isCommand name message = (name `T.append` " ") `T.isPrefixOf` message

toArgs :: T.Text -> T.Text -> [T.Text]
toArgs name = T.words . T.drop (T.length name + 1)
