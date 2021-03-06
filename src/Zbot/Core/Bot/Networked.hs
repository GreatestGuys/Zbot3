{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Zbot.Metrics

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Time (getCurrentTime)

import qualified Control.Concurrent.Chan as Chan
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.Connection as Network
import qualified Prometheus as P


data NetworkState = NetworkState {
    connection     :: Network.Connection
,   messageChannel :: Chan.Chan Message
,   verboseLogging :: Bool
}

type NetworkedBot = IOCollective (StateT NetworkState (StateT EngineState IO))

-- | The maximum number of `BestEffort` messages to send per second.
type RateLimit = Int

instance MonadState EngineState NetworkedBot where
    get = lift $ lift get
    put = lift . lift . put

instance Irc NetworkedBot where
    sendMessage RealTime message = do
        socket <- lift $ gets connection
        liftIO $ Network.connectionPut socket $ T.encodeUtf8 (render message)
        P.withLabel metricSentMessages "real-time" P.incCounter
        verboseLogging <- lift $ gets verboseLogging
        when verboseLogging $ liftIO $ T.putStr $ T.concat [
                "[->IRC] "
            ,   "(", T.pack $ show RealTime, ") "
            ,   render message
            ]
    sendMessage BestEffort message = do
        messageChannel <- lift $ gets messageChannel
        liftIO $ Chan.writeChan messageChannel message
        verboseLogging <- lift $ gets verboseLogging
        P.withLabel metricSentMessages "best-effort" P.incCounter
        when verboseLogging $ liftIO $ T.putStr $ T.concat [
                "[->IRC] "
            ,   "(", T.pack $ show BestEffort, ") "
            ,   render message
            ]

instance Bot NetworkedBot where

runNetworkedBot :: Server
                -> Port
                -> Nick
                -> User
                -> Password
                -> FilePath
                -> RateLimit
                -> Bool -- ^ verboseLogging
                -> Bool -- ^ useSsl
                -> NetworkedBot ()
                -> IO ()
runNetworkedBot server port nick user password dataDir rateLimit verboseLogging
                useSsl botInit = do
    context <- Network.initConnectionContext
    let sslParams = Network.TLSSettingsSimple {
        Network.settingDisableCertificateValidation = False,
        Network.settingDisableSession               = False,
        Network.settingUseServerName                = False
    }
    let connectionParams = Network.ConnectionParams {
        Network.connectionHostname  = server,
        Network.connectionPort      = fromIntegral port,
        Network.connectionUseSecure = guard useSsl >> Just sslParams,
        Network.connectionUseSocks  = Nothing
    }
    socket <- Network.connectTo context connectionParams
    messageChannel <- Chan.newChan
    forkIO (writeLoop socket messageChannel rateLimit)
    flip evalStateT undefined $
        flip evalStateT (NetworkState socket messageChannel verboseLogging) $
            runIOCollective dataDir $ do
                startEngine nick user password
                botInit
                forever processInput

processInput :: NetworkedBot ()
processInput = do
    socket <- lift $ gets connection
    verboseLogging <- lift $ gets verboseLogging
    void $ runMaybeT $ do
        rawMessage <- liftIO $ safeHGetLine socket
        message <- MaybeT $ return $ parse $ rawMessage `T.append` "\x0a"
        P.incCounter metricReceivedMessages
        when verboseLogging $ liftIO $ T.putStr $ T.concat [
                "[<-IRC] "
            ,   render message
            ]
        events <- lift $ stepEngine message
        timestamp <- Time <$> liftIO getCurrentTime
        lift $ mapM_ processEvent (timestamp:events)

writeLoop :: Network.Connection -> Chan.Chan Message -> RateLimit -> IO ()
writeLoop socket messageChannel rateLimit = forever $ do
    message <- Chan.readChan messageChannel
    Network.connectionPut socket (T.encodeUtf8 $ render message)
    when (rateLimit > 0) $ threadDelay (1000000 `div` rateLimit)

safeHGetLine :: MonadIO io => Network.Connection -> io T.Text
safeHGetLine handle = do
    line <- liftIO $ Network.connectionGetLine maxBound handle
    -- If unable to decode this line, move on to the next one.
    either (const $ safeHGetLine handle) return $ T.decodeUtf8' line
