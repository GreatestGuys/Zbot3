module Zbot.Extras.Message (
    Reply
,   MessageContext(..)
,   MessageSource(..)
,   onMessage
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service

import qualified Data.Text as T


-- | A function that can be used to send a message to either the channel or the
-- nick who initiated the command.
type Reply m = T.Text -> m ()

data MessageSource = ShoutSource Channel Nick
                   | WhisperSource Nick

data MessageContext m = MessageContext {
  -- | Reply to the message via the same mechanism that it was received.
  reply           :: Reply m
  -- | Reply to the message by shouting on a channel. If the message was
  -- received in a PM, the reply is dropped.
, shoutBackOrDrop :: Reply m
  -- | Reply to the message by whispering to the nick that sent the message.
, whisperBack     :: Reply m
  -- | The source of the message, either the channel the message came in on, or
  -- the nick of the user that whispered it.
, source :: MessageSource
}

onMessage :: Bot m
          => (MessageContext m -> T.Text -> MonadService s m ())
          -> Event -> MonadService s m ()
onMessage handler (Shout channel nick msg) = handler MessageContext {
    reply           = shout channel
  , shoutBackOrDrop = shout channel
  , whisperBack     = whisper nick
  , source          = ShoutSource channel nick
  } msg
onMessage handler (Whisper nick msg)       = handler MessageContext {
    reply           = whisper nick
  , shoutBackOrDrop = \_ -> return ()
  , whisperBack     = whisper nick
  , source          = WhisperSource nick
  } msg
onMessage _       _                        = return ()
