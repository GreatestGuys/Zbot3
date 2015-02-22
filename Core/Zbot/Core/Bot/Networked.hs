{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Zbot.Core.Bot.Networked (
    NetworkedBot
,   runNetworkedBot
) where

import Zbot.Core.Bot.Types
import Zbot.Core.Irc
import Zbot.Core.Irc.Engine
import Zbot.Core.Irc.Protocol
import Zbot.Core.Service.IO
import Zbot.Core.Service.Types hiding (Handle)

import Control.Monad.State
import Control.Monad.Trans.Maybe
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network


data NetworkState = NetworkState {
    socket :: Handle
}

type NetworkedBot = IOCollective (StateT NetworkState (StateT EngineState IO))

instance MonadState EngineState NetworkedBot where
    get = lift $ lift get
    put = lift . lift . put

instance Irc NetworkedBot where
    sendMessage _ message = do
        socket <- lift $ gets socket
        liftIO $ T.hPutStr socket (render message)

instance Bot NetworkedBot where

runNetworkedBot :: Server -> Port -> Nick -> User -> NetworkedBot () -> IO ()
runNetworkedBot server port nick user botInit = do
    socket <- Network.connectTo server $ Network.PortNumber (fromIntegral port)
    hSetBuffering socket NoBuffering
    hSetEncoding socket utf8
    flip evalStateT undefined $
        flip evalStateT (NetworkState socket) $
            runIOCollective $ do
                startEngine nick user
                botInit
                forever processInput

processInput :: NetworkedBot ()
processInput = do
    socket <- lift $ gets socket
    void $ runMaybeT $ do
        rawMessage <- liftIO $ T.hGetLine socket
        message <- MaybeT $ return $ parse $ rawMessage `T.append` "\x0a"
        events <- lift $ stepEngine message
        lift $ mapM_ processEvent events
