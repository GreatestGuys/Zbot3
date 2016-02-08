{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Zbot.Extras.Command (
    Reply
,   onCommand
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Message

import qualified Data.Text as T


onCommand :: Bot m
          => T.Text
          -> (Reply m -> T.Text -> MonadService s m ())
          -> Event -> MonadService s m ()
onCommand name handler = onMessage $ \reply msg -> if
    | isCommand name msg -> handler reply (toArgs name msg)
    | otherwise          -> return ()

isCommand :: T.Text -> T.Text -> Bool
isCommand name message = (name `T.append` " ") `T.isPrefixOf` message
                       || name == message

toArgs :: T.Text -> T.Text -> T.Text
toArgs name = T.drop (T.length name + 1)
