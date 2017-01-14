module Zbot.Extras.Message (
    Reply
,   onMessage
,   onMessageWithChannel
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service

import qualified Data.Text as T


-- | A function that can be used to send a message to either the channel or the
-- nick who initiated the command.
type Reply m = T.Text -> m ()

onMessage :: Bot m
          => (Reply m -> T.Text -> MonadService s m ())
          -> Event -> MonadService s m ()
onMessage handler (Shout channel _ msg) = handler (shout channel) msg
onMessage handler (Whisper nick msg)    = handler (whisper nick) msg
onMessage _       _                     = return ()

-- | A utility function for onMessage callers that want access to the their
-- current channel.
onMessageWithChannel :: Bot m
                     => (Channel -> Reply m -> T.Text -> MonadService s m ())
                     -> Event -> MonadService s m ()
onMessageWithChannel handler e@(Shout channel _ _) = onMessage (handler channel) e
onMessageWithChannel _       _                     = return ()
