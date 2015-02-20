module Zbot.Core.Irc.Types (
    Server
,   Port

,   Nick
,   User
,   Host
,   Channel
,   ChannelMode (..)
,   ModeDiff (..)

,   Event (..)
) where

import qualified Data.Text as T


type Server = String

type Port = Int

type Nick = T.Text

type User = T.Text

type Host = T.Text

type Channel = T.Text

data ChannelMode
    = Op Nick
    | Voice Nick
    | Secret
    deriving (Eq, Ord, Show, Read)

data ModeDiff a = AddMode a | RmMode a
    deriving (Eq, Ord, Show, Read)

data Event
    = Shout Channel T.Text
    | Whisper Nick T.Text
    | Join Channel Nick
    | Part Channel Nick
    deriving (Eq, Ord, Show, Read)
