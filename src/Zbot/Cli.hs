module Zbot.Cli (
    zbotMain
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc

import Options.Applicative

import qualified Data.Text as T


data Options = Options {
            serverFlag   ::  Server
        ,   portFlag     ::  Port
        ,   nickFlag     ::  Nick
        ,   channelsFlag ::  [Channel]
        ,   dataDirFlag  ::  FilePath
        ,   msgRateFlag  ::  Int
    }

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (helper <*> optionsParser) meta
    where
        optionsParser :: Parser Options
        optionsParser = Options
            <$> strOption (long "server"
                <> metavar "HOST"
                <> help "The IRC server to connect to")
            <*> option auto (long "port"
                <> metavar "PORT"
                <> value 6667
                <> help "The port the server is listening on (default: 6667)")
            <*> textOption (long "nick"
                <> metavar "NICK"
                <> value "zbot3"
                <> help "The nick that the bot should use (default: zbot3)")
            <*> some (textOption (long "channel"
                <> metavar "CHANNEL,..."
                <> help "The channels that the bot should join"))
            <*> strOption (long "data"
                <> metavar "DIRECTORY"
                <> help "The directory to store service data files in")
            <*> option auto (long "rate-limit"
                <> metavar "INT"
                <> value 10
                <> help "The maximum messages to send per second (default: 10)")

        meta = fullDesc
             <> header "zbot - An IRC bot framework."

zbotMain :: NetworkedBot () -> IO ()
zbotMain init = execParser optionsParserInfo >>= \options ->
    runNetworkedBot
        (serverFlag options)
        (portFlag options)
        (nickFlag options)
        (nickFlag options)  -- Use the same value for user.
        (dataDirFlag options)
        (msgRateFlag options)
        (mapM_ joinChannel (channelsFlag options) >> init)