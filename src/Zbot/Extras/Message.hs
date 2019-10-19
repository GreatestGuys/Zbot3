module Zbot.Extras.Message (
    Reply
,   ReplyMode(..)
,   onMessage
,   onMessageWithChannel
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service

import qualified Data.Text as T


-- | A function that can be used to send a message to either the channel or the
-- nick who initiated the command.
type Reply m = ReplyMode -> T.Text -> m ()

data ReplyMode = Direct | ForceWhisper | ShoutOrDrop

onMessage :: Bot m
          => (Reply m -> T.Text -> MonadService s m ())
          -> Event -> MonadService s m ()
onMessage handler (Shout channel nick msg) =
  let reply Direct       = shout channel
      reply ShoutOrDrop  = shout channel
      reply ForceWhisper = whisper nick
  in handler reply msg
onMessage handler (Whisper nick msg) =
  let reply Direct       = whisper nick
      reply ShoutOrDrop  = \_ -> return ()
      reply ForceWhisper = whisper nick
  in handler reply msg
onMessage _       _                  = return ()

-- | A utility function for onMessage callers that want access to the their
-- current channel.
onMessageWithChannel :: Bot m
                     => (Channel -> Reply m -> T.Text -> MonadService s m ())
                     -> Event -> MonadService s m ()
onMessageWithChannel handler e@(Shout channel _ _) = onMessage (handler channel) e
onMessageWithChannel _       _                     = return ()
