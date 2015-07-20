module Zbot.Core.Irc (
    Irc
,   Event (..)

,   Server
,   Port
,   Nick
,   User
,   Channel
,   ChannelMode (..)

,   myNick
,   nicks
,   channels
,   whisper
,   shout
,   joinChannel
,   partChannel
,   channelMode
,   addChannelMode
,   rmChannelMode
) where

import Zbot.Core.Irc.Engine
import Zbot.Core.Irc.Protocol
import Zbot.Core.Irc.Types

import Control.Applicative
import Control.Monad.State
import Data.Maybe

import qualified Data.Map.Strict as Map
import qualified Data.Text as T


-- | The current nick of the bot.
myNick :: Irc irc => irc Nick
myNick = gets engineStateNick

-- | A list of all nicks in a given channel.
nicks :: Irc irc => Channel -> irc [Nick]
nicks channel = do
    maybeChannel <- getMaybeChannel channel
    return $ fromMaybe [] (channelStateNicks <$> maybeChannel)

-- | A list of all channels that are currently joined.
channels :: Irc irc => irc [Channel]
channels = gets (Map.keys . engineStateChannels)

-- | Send a private message to a given nick.
whisper :: Irc irc => Nick -> T.Text -> irc ()
whisper = shout

-- | Send a message to a given channel.
shout :: Irc irc => Channel -> T.Text -> irc ()
shout channel message
    | T.null message = sendPrivMsg " "
    | otherwise      = mapM_ sendPrivMsg chunks
    where
        sendPrivMsg = sendMessage BestEffort . toPrivMessage
        chunks = T.chunksOf (512 - 128) message
        toPrivMessage msg = Message {
            prefix     = Nothing
        ,   command    = "PRIVMSG"
        ,   parameters = [channel]
        ,   trailing   = Just msg
        }

-- | Join an IRC channel.
joinChannel :: Irc irc => Channel -> irc ()
joinChannel channel = do
    sendMessage BestEffort joinMessage
    sendMessage BestEffort namesMessage
    modify addChannel
    where
        baseMessage  = Message {
            prefix     = Nothing
        ,   command    = ""
        ,   parameters = [channel]
        ,   trailing   = Nothing
        }
        joinMessage  = baseMessage {command = "JOIN"}
        namesMessage = baseMessage {command = "NAMES"}
        addChannel engineState = engineState {
            engineStateChannels = Map.insert
                channel
                emptyChannelState
                (engineStateChannels engineState)
        }

-- | Part an IRC channel.
partChannel :: Irc irc => Channel -> irc ()
partChannel channel = do
    sendMessage BestEffort partMessage
    modify removeChannel
    where
        partMessage = Message {
            prefix     = Nothing
        ,   command    = "PART"
        ,   parameters = [channel]
        ,   trailing   = Nothing
        }
        removeChannel engineState = engineState {
            engineStateChannels =
                Map.delete channel (engineStateChannels engineState)
        }

-- | The modes that a given channel has.
channelMode :: Irc irc => Channel -> irc [ChannelMode]
channelMode channel = do
    maybeChannel <- getMaybeChannel channel
    return $ fromMaybe [] (channelStateMode <$> maybeChannel)

-- | Add a list of modes to a channel.
addChannelMode :: Irc irc => Channel -> [ChannelMode] -> irc ()
addChannelMode channel = mapM_ (sendModeMessage channel . AddMode)

-- | Remove a list of modes from a channel.
rmChannelMode :: Irc irc => Channel -> [ChannelMode] -> irc ()
rmChannelMode channel = mapM_ (sendModeMessage channel . RmMode)

sendModeMessage :: (Irc irc)
                => Channel
                -> ModeDiff ChannelMode
                -> irc ()
sendModeMessage channel diff = sendMessage BestEffort modeMessage
    where
        modeMessage = Message {
            prefix     = Nothing
        ,   command    = "MODE"
        ,   parameters = channel : renderDiff diff
        ,   trailing   = Nothing
        }

        renderDiff (AddMode mode) = renderMode "+" mode
        renderDiff (RmMode mode)  = renderMode "-" mode

        renderMode prefix (Op nick)    = [prefix `T.append` "o", nick]
        renderMode prefix (Voice nick) = [prefix `T.append` "v", nick]
        renderMode prefix (Secret)     = [prefix `T.append` "s"]

getMaybeChannel :: Irc irc => Channel -> irc (Maybe ChannelState)
getMaybeChannel channel = gets (Map.lookup channel . engineStateChannels)
