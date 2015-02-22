module Zbot.Extras.Message (
    Reply
,   onMessage
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc

import Control.Monad.State

import qualified Data.Text as T

-- | A function that can be used to send a message to either the channel or the
-- nick who initiated the command.
type Reply m = T.Text -> m ()

onMessage :: Bot m
          => (Reply m -> T.Text -> StateT s m ())
          -> Event -> StateT s m ()
onMessage handler (Shout channel _ msg) = handler (shout channel) msg
onMessage handler (Whisper nick msg)    = handler (whisper nick) msg
onMessage _       _                     = return ()
