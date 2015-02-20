{-# LANGUAGE FlexibleContexts #-}
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

import qualified Data.Map.Strict as Map


data Priority = RealTime | BestEffort
    deriving (Eq, Ord, Show, Read)

data ChannelState = ChannelState {
    channelStateNicks :: [Nick]
,   channelStateMode  :: [ChannelMode]
} deriving (Eq, Ord, Show, Read)

data EngineState = EngineState {
    engineStateChannels :: Map.Map Channel ChannelState
,   engineStateNick     :: Nick
,   engineStateUser     :: User
} deriving (Eq, Ord, Read, Show)

-- | A Monad that is capable of running an IRC engine.
class (Monad r, MonadState EngineState r) => Irc r where

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
        state       = EngineState Map.empty nick user
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

-- | Processes an IRC messages given an EngineState and produces a list of IRC
-- messages of differing priorities and a new EngineState.
stepEngine :: Irc irc => Message -> irc [Event]
stepEngine message = execWriterT $ mapM_ ($ message) defaultHandlers

-- | A list of handlers that are responsible for processing specific IRC
-- messages, producing any relevant outgoing messages and updating the engine's
-- state.
defaultHandlers :: Irc irc => [Message -> WriterT [Event] irc ()]
defaultHandlers = [
        pingPongHandler
    ]

-- | A handler that handles incoming PING commands and responds with a PONG
-- message.
pingPongHandler :: Irc irc => Message -> WriterT [Event] irc ()
pingPongHandler msg@Message{command="PING"} =
    lift $ sendMessage RealTime msg {command = "PONG"}
pingPongHandler _                           = return ()
