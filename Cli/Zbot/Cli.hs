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
    }

textOption :: Mod OptionFields String -> Parser T.Text
textOption = (fmap T.pack) . strOption

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info optionsParser meta
    where
        optionsParser :: Parser Options
        optionsParser = Options
            <$> strOption (long "server"
                <> help "The IRC server to connect to")
            <*> option auto (long "port"
                <> value 6667
                <> help "The port on which the IRC server is running")
            <*> textOption (long "nick"
                <> value "zbot3"
                <> help "The nick that the bot should connect as")
            <*> some (textOption (long "channel"
                <> help "The channels that the bot should join"))

        meta = fullDesc
             <> header "zbot - An IRC bot framework."

zbotMain :: NetworkedBot () -> IO ()
zbotMain init = execParser optionsParserInfo >>= \options ->
    runNetworkedBot
        (serverFlag options)
        (portFlag options)
        (nickFlag options)
        (nickFlag options)
        (mapM_ joinChannel (channelsFlag options) >> init)
