{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Zbot.Core.Bot.Networked (
    NetworkedBot
,   RateLimit
,   runNetworkedBot
) where

import Zbot.Core.Bot.Types
import Zbot.Core.Irc
import Zbot.Core.Irc.Engine
import Zbot.Core.Irc.Protocol
import Zbot.Core.Service.IO
import Zbot.Core.Service.Types hiding (Handle)

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import System.IO

import qualified Control.Concurrent.Chan as Chan
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network


data NetworkState = NetworkState {
    socket         :: Handle
,   messageChannel :: Chan.Chan Message
}

type NetworkedBot = IOCollective (StateT NetworkState (StateT EngineState IO))

-- | The maximum number of `BestEffort` messages to send per second.
type RateLimit = Int

instance MonadState EngineState NetworkedBot where
    get = lift $ lift get
    put = lift . lift . put

instance Irc NetworkedBot where
    sendMessage RealTime message = do
        socket <- lift $ gets socket
        liftIO $ T.hPutStr socket (render message)
--        liftIO $ T.putStr $ T.concat [
--                "[->IRC] "
--            ,   "(", T.pack $ show RealTime, ") "
--            ,   render message
--            ]
    sendMessage BestEffort message = do
        messageChannel <- lift $ gets messageChannel
        liftIO $ Chan.writeChan messageChannel message
--        liftIO $ T.putStr $ T.concat [
--                "[->IRC] "
--            ,   "(", T.pack $ show BestEffort, ") "
--            ,   render message
--            ]

instance Bot NetworkedBot where

runNetworkedBot :: Server
                -> Port
                -> Nick
                -> User
                -> FilePath
                -> RateLimit
                -> NetworkedBot ()
                -> IO ()
runNetworkedBot server port nick user dataDir rateLimit botInit = do
    socket <- Network.connectTo server $ Network.PortNumber (fromIntegral port)
    hSetBuffering socket NoBuffering
    hSetEncoding socket utf8
    messageChannel <- Chan.newChan
    forkIO (writeLoop socket messageChannel rateLimit)
    flip evalStateT undefined $
        flip evalStateT (NetworkState socket messageChannel) $
            runIOCollective dataDir $ do
                startEngine nick user
                botInit
                forever processInput

processInput :: NetworkedBot ()
processInput = do
    socket <- lift $ gets socket
    void $ runMaybeT $ do
        rawMessage <- liftIO $ safeHGetLine socket
        message <- MaybeT $ return $ parse $ rawMessage `T.append` "\x0a"
--        liftIO $ T.putStr $ T.concat [
--                "[<-IRC] "
--            ,   render message
--            ]
        events <- lift $ stepEngine message
        lift $ mapM_ processEvent events

writeLoop :: Handle -> Chan.Chan Message -> RateLimit -> IO ()
writeLoop socket messageChannel rateLimit = forever $ do
    message <- Chan.readChan messageChannel
    T.hPutStr socket (render message)
    threadDelay (1000000 `div` rateLimit)

safeHGetLine :: MonadIO io => Handle -> io T.Text
safeHGetLine handle = do
    line <- liftIO $ BS.hGetLine handle
    -- If unable to decode this line, move on to the next one.
    either (const $ safeHGetLine handle) return $ T.decodeUtf8' line
