module Zbot.Core.Irc.Types (
    Server
,   Port

,   Nick
,   User
,   Password
,   Channel
,   ChannelMode (..)
,   ModeDiff (..)

,   Event (..)
) where

import qualified Data.Text as T
import Data.Time (UTCTime)


type Server = String

type Port = Int

type Nick = T.Text

type User = T.Text

type Password = T.Text

type Channel = T.Text

data ChannelMode
    = Op Nick
    | Voice Nick
    | Secret
    deriving (Eq, Ord, Show, Read)

data ModeDiff a = AddMode a | RmMode a
    deriving (Eq, Ord, Show, Read)

data Event
    = Shout Channel Nick T.Text  -- ^ A nick messaging a channel.
    | Whisper Nick T.Text  -- ^ A nick messaging this bot.
    | Invite Nick Channel -- ^ A channel invite.
    | Join Channel Nick
    | Part Channel Nick
    | NickChange Nick Nick  -- ^ A change from the first nick to the second.
    | Initialize  -- ^ An event that is broadcast when the bot first connects to
                  --   an IRC server. This event may be broadcast multiple times
                  --   if the bot connects multiple times.
    | Time UTCTime
    deriving (Eq, Ord, Show, Read)
