{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Zbot.Core.Irc.Engine (
    ChannelState (..)
,   EngineState (..)
,   Priority (..)

,   Irc (..)
,   startEngine
,   stepEngine

,   emptyChannelState
) where

import Zbot.Core.Irc.Protocol
import Zbot.Core.Irc.Types

import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe (maybeToList)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T


data Priority = RealTime | BestEffort
    deriving (Eq, Ord, Show, Read)

data ChannelState = ChannelState {
    channelStateNicks :: [Nick]
,   channelStateMode  :: [ChannelMode]
} deriving (Eq, Ord, Show, Read)

data EngineState = EngineState {
    engineStateChannels     :: Map.Map Channel ChannelState
,   engineStateChannelsTemp :: Map.Map Channel ChannelState
,   engineStateNick         :: Nick
,   engineStateUser         :: User
} deriving (Eq, Ord, Read, Show)

-- | A Monad that is capable of running an IRC engine.
class (Applicative r,
       Functor r,
       Monad r,
       MonadState EngineState r) => Irc r where

    -- | Send an IRC message with a given priority.
    sendMessage :: Priority -> Message -> r ()

-- | The initial state that is given to a channel when it has just been join and
-- before any information has been returned by the IRC server.
emptyChannelState :: ChannelState
emptyChannelState = ChannelState [] []

-- | Generates the initial engine state and outgoing messages that are
-- responsible for bringing up the IRC engine.
startEngine :: Irc irc => Nick -> User -> irc ()
startEngine nick user = put state >> mapM_ (sendMessage RealTime) outgoing
    where
        state       = EngineState Map.empty Map.empty nick user
        outgoing    = [nickMessage, userMessage]
        nickMessage = Message {
                prefix     = Nothing
            ,   command    = "NICK"
            ,   parameters = [nick]
            ,   trailing   = Nothing
            }
        userMessage = Message {
                prefix     = Nothing
            ,   command    = "USER"
            ,   parameters = [nick, "0", "*"]
            ,   trailing   = Just "Zbot v3"
            }

-- | Processes IRC messages given an EngineState and produces a list of IRC
-- messages of differing priorities and a new EngineState.
stepEngine :: Irc irc => Message -> irc [Event]
stepEngine message = execWriterT $ mapM_ ($ message) handlers

--------------------------------------------------------------------------------
-- Handlers for various IRC messages.

-- | A list of handlers that are responsible for processing specific IRC
-- messages, producing any relevant outgoing messages and updating the engine's
-- state.
handlers :: Irc irc => [Message -> WriterT [Event] irc ()]
handlers = [
        inviteHandler
    ,   joinHandler
    ,   nickHandler
    ,   partHandler
    ,   pingPongHandler
    ,   privmsgHandler
    ,   welcomeHandler
    ,   stalkerHandler
    ]

-- | A handler that handles incoming PING messages and responds with a PONG
-- message.
pingPongHandler :: Irc irc => Message -> WriterT [Event] irc ()
pingPongHandler msg@Message{command="PING"} =
    lift $ sendMessage RealTime msg {command = "PONG"}
pingPongHandler _                           = return ()

inviteHandler :: Irc irc => Message -> WriterT [Event] irc ()
inviteHandler Message {
              command    = "INVITE"
          ,   parameters = [invitee, channel]
          } = tell [Invite invitee channel]
inviteHandler _ = return ()

nickHandler :: Irc irc => Message -> WriterT [Event] irc ()
nickHandler Message {
            command    = "NICK"
        ,   prefix     = Just ClientPrefix{nick=oldNick}
        ,   parameters = [newNick]
        } = do
    tell [NickChange oldNick newNick]
    lift $ modify (modifyChannels updateNick)
    where
        updateNick channelState = channelState {
                channelStateNicks = map swapNick
                                  $ channelStateNicks channelState
            }
        swapNick n | n == oldNick = newNick
                   | otherwise    = n
nickHandler _ = return ()

joinHandler :: Irc irc => Message -> WriterT [Event] irc ()
joinHandler Message {
            command    = "JOIN"
        ,   prefix     = Just ClientPrefix{nick=nick}
        ,   parameters = [channel]
        } = do
    tell [Join channel nick]
    lift $ modify (modifyChannel channel addNick)
    where
        addNick channelState = channelState {
                channelStateNicks = nick : channelStateNicks channelState
            }
joinHandler _ = return ()

partHandler :: Irc irc => Message -> WriterT [Event] irc ()
partHandler Message {
            command    = "PART"
        ,   prefix     = Just ClientPrefix{nick=nick}
        ,   parameters = [channel]
        } = do
    tell [Part channel nick]
    lift $ modify (modifyChannel channel rmNick)
    where
        rmNick channelState = channelState {
                channelStateNicks = filter (/= nick)
                                  $ channelStateNicks channelState
            }
partHandler _ = return ()

privmsgHandler :: Irc irc => Message -> WriterT [Event] irc ()
privmsgHandler Message {
            command    = "PRIVMSG"
        ,   prefix     = Just ClientPrefix{nick=nick}
        ,   parameters = [target]
        ,   trailing   = Just message
        } | T.isPrefixOf "#" target = tell [Shout target nick message]
          | T.isPrefixOf "&" target = tell [Shout target nick message]
          | T.isPrefixOf "+" target = tell [Shout target nick message]
          | T.isPrefixOf "!" target = tell [Shout target nick message]
          | otherwise               = tell [Whisper nick message]
privmsgHandler _ = return ()

welcomeHandler :: Irc irc => Message -> WriterT [Event] irc ()
welcomeHandler Message {command="001"} = tell [Initialize]
welcomeHandler _                       = return ()

stalkerHandler :: Irc irc => Message -> WriterT [Event] irc ()
stalkerHandler Message {command = "QUIT"} = lift sendNames
stalkerHandler Message {command = "PART"} = lift sendNames
stalkerHandler Message {command = "NICK"} = lift sendNames
stalkerHandler Message {command = "JOIN"} = lift sendNames
stalkerHandler Message {command = "MODE"} = lift sendNames
stalkerHandler Message {command = "366", parameters = (_:channel:_)} =
    lift $ finishAddingNicksChannel channel
stalkerHandler Message {
            command    = "353"
        ,   parameters = (_:_:channel:nicks)
        ,   trailing   = trailing
        } = lift $ addNicksToChannel channel (nicks ++ maybeToList trailing)
stalkerHandler _                          = return ()

sendNames :: Irc irc => irc ()
sendNames = channels >>= mapM_ sendName
    where
        sendName ch = sendMessage BestEffort Message {
            prefix     = Nothing
        ,   command    = "NAMES"
        ,   parameters = [ch]
        ,   trailing   = Nothing
        }
        channels = gets (Map.keys . engineStateChannels)

addNicksToChannel :: Irc irc => Channel -> [Nick] -> irc ()
addNicksToChannel channel = mapM_ (modify . modifyTempChannel channel . addNick)
    where
        addNick nick channelState = channelState {
                channelStateNicks = formatNick nick
                                  : channelStateNicks channelState
            }
        formatNick nick
            | "@" `T.isPrefixOf` nick = T.drop 1 nick
            | "+" `T.isPrefixOf` nick = T.drop 1 nick
            | otherwise               = nick

finishAddingNicksChannel :: Irc irc => Channel -> irc ()
finishAddingNicksChannel channel = modify (swapChannelWithTemp channel)

--------------------------------------------------------------------------------
-- Handler utility methods.

modifyChannels :: (ChannelState -> ChannelState)
               -> EngineState -> EngineState
modifyChannels f engineState = engineState {
        engineStateChannels = f <$> engineStateChannels engineState
    }

modifyChannel :: Channel
              -> (ChannelState -> ChannelState)
              -> EngineState -> EngineState
modifyChannel channel f engineState = engineState {
        engineStateChannels = adjustWithDefault emptyChannelState f channel
                            $ engineStateChannels engineState
    }

modifyTempChannel :: Channel
                  -> (ChannelState -> ChannelState)
                  -> EngineState -> EngineState
modifyTempChannel channel f engineState = engineState {
        engineStateChannelsTemp = adjustWithDefault emptyChannelState f channel
                                $ engineStateChannelsTemp engineState
    }

swapChannelWithTemp :: Channel -> EngineState -> EngineState
swapChannelWithTemp channel engineState = engineState {
        engineStateChannels     = Map.insert channel newChannelState
                                $ engineStateChannels engineState
    ,   engineStateChannelsTemp = Map.delete channel
                                $ engineStateChannelsTemp engineState
    }
    where newChannelState = Map.findWithDefault
            emptyChannelState
            channel
            (engineStateChannelsTemp engineState)

adjustWithDefault :: Ord k => v -> (v -> v) -> k -> Map.Map k v -> Map.Map k v
adjustWithDefault initial f = Map.alter f'
    where
        f' Nothing  = Just $ f initial
        f' (Just v) = Just $ f v

