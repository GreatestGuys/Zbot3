{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Zbot.Extras.Command (
    Reply
,   MessageContext(..)
,   MessageSource(..)
,   onCommand
,   onCommands
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Message

import qualified Data.Foldable as F
import qualified Data.Text as T


onCommand :: Bot m
          => T.Text
          -> (MessageContext m -> T.Text -> MonadService s m ())
          -> (Event -> MonadService s m ())
onCommand name handler = onMessage $ \reply msg -> if
    | isCommand name msg -> handler reply (toArgs name msg)
    | otherwise          -> return ()

onCommands :: Bot m
           => [T.Text]
           -> (MessageContext m -> T.Text -> MonadService s m ())
           -> (Event -> MonadService s m ())
onCommands names handler = onMessage $ \reply msg ->
    let
        name = F.find (`isCommand` msg) names
    in
        case name of
            (Just name) -> handler reply (toArgs name msg)
            Nothing -> return ()

isCommand :: T.Text -> T.Text -> Bool
isCommand name message = (name `T.append` " ") `T.isPrefixOf` message
                       || name == message

toArgs :: T.Text -> T.Text -> T.Text
toArgs name = T.drop (T.length name + 1)
