module Zbot.Extras.Command (
    Reply
,   command
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc

import Control.Monad.State

import qualified Data.Text as T

-- | A function that can be used to send a message to either the channel or the
-- nick who initiated the command.
type Reply m = T.Text -> m ()

command :: Bot m
        => T.Text
        -> (Reply m -> [T.Text] -> StateT s m ())
        -> Event -> StateT s m ()
command name handler (Shout channel _ msg)
    | isCommand name msg = handler (shout channel) (toArgs name msg)
command name handler (Whisper nick msg)
    | isCommand name msg = handler (whisper nick) (toArgs name msg)
command _    _       _   = return ()

isCommand :: T.Text -> T.Text -> Bool
isCommand name message = (name `T.append` " ") `T.isPrefixOf` message

toArgs :: T.Text -> T.Text -> [T.Text]
toArgs name = T.words . T.drop (T.length name + 1)
