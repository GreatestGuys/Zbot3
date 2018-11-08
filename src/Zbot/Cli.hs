{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module Zbot.Cli (
    zbotMain
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc

import Control.Concurrent (forkIO)
import Control.Monad (when, void)
import Data.Semigroup ((<>))
import Network.Wai.Middleware.Prometheus (metricsApp)
import Options.Applicative
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)

import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp


data Options = Options {
            serverFlag      ::  Server
        ,   portFlag        ::  Port
        ,   nickFlag        ::  Nick
        ,   userFlag        ::  User
        ,   passwordFlag    ::  Password
        ,   channelsFlag    ::  [Channel]
        ,   dataDirFlag     ::  FilePath
        ,   msgRateFlag     ::  Int
        ,   verboseFlag     ::  Bool
        ,   sslFlag         ::  Bool
        ,   metricsPortFlag ::  Int
    }

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (helper <*> optionsParser) meta
    where
        optionsParser :: Parser Options
        optionsParser = do
            serverFlag <- strOption (long "server"
                       <> metavar "HOST"
                       <> help "The IRC server to connect to")
            portFlag <- option auto (long "port"
                     <> metavar "PORT"
                     <> value 6667
                     <> help "The port the server is listening on (default: 6667)")
            nickFlag <- textOption (long "nick"
                     <> metavar "NICK"
                     <> value "zbot3"
                     <> help "The nick that the bot should use (default: zbot3)")
            userFlag <- textOption (long "user"
                     <> metavar "USER"
                     <> value ""
                     <> help "The user the bot should use (default: same as nick)")
            passwordFlag <- textOption (long "password"
                         <> metavar "PASSWORD"
                         <> value ""
                         <> help "The password the bot should connect with")
            channelsFlag <- some (textOption (long "channel"
                         <> metavar "CHANNEL,..."
                         <> help "The channels that the bot should join"))
            dataDirFlag <- strOption (long "data"
                        <> metavar "DIRECTORY"
                        <> help "The directory to store service data files in")
            msgRateFlag <- option auto (long "rate-limit"
                        <> metavar "INT"
                        <> value 10
                        <> help "The maximum messages to send per second (default: 10)")
            verboseFlag <- switch (long "verbose"
                        <> short 'v'
                        <> help "Enable verbose logging.")
            sslFlag <- switch (long "ssl"
                    <> help "Connect to the IRC server over SSL.")
            metricsPortFlag <- option auto (long "metrics-port"
                            <> metavar "METRICS-PORT"
                            <> value 0
                            <> help (
                              "If non-zero, the port that the prometheus " ++
                              "metric endpoint will listen on (default: 0)"))

            pure Options{..}

        meta = fullDesc
             <> header "zbot - An IRC bot framework."

chooseUser nick user | T.null user = nick
                     | otherwise   = user

zbotMain :: NetworkedBot () -> IO ()
zbotMain init = execParser optionsParserInfo >>= \options -> do
    when (metricsPortFlag options > 0) $ do
        register ghcMetrics
        void $ forkIO $ Warp.run (metricsPortFlag options) metricsApp
    runNetworkedBot
        (serverFlag options)
        (portFlag options)
        (nickFlag options)
        (chooseUser (nickFlag options) (userFlag options))
        (passwordFlag options)
        (dataDirFlag options)
        (msgRateFlag options)
        (verboseFlag options)
        (sslFlag options)
        (mapM_ joinChannel (channelsFlag options) >> init)
