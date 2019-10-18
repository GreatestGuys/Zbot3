{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Zbot.Service.Grep (
    grep
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Color
import Zbot.Extras.Message
import Zbot.Extras.UnitService
import Zbot.Service.History

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.String (fromString)
import Data.Time.Clock (UTCTime)
import Options.Applicative
import Options.Applicative.Help.Core (parserHelp)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

import qualified Data.Text as T


grep :: (MonadIO m, Bot m) => Handle m History -> Service m ()
grep historyHandle =
    (unitService "Zbot.Service.Grep" $ onMessageWithChannel handler) {
        helpSpec = Just HelpSpec {
                helpAliases = ["!grep"]
            ,   helpMessage = T.lines
                            . T.pack
                            . show
                            $ parserHelp defaultPrefs (mkGrepParser "")
            }
    }
    where
        handler channel reply msg
            | ("!grep":rest) <- args = grepCommand channel reply rest
            | otherwise              = return ()
            where
                args = T.words msg

        grepCommand channel reply args = lift $
            case parseGrepOptions parser args of
                Nothing   -> reply "Failed to parse !grep"
                Just opts -> do
                    history <-  drop 1
                            <$> foldHistoryForward historyHandle historyToList []
                    let matches = match opts [] history
                    case matches of
                        [] -> reply "No matches found."
                        _  -> mapM_ reply
                            $ reverse
                            $ intercalate [colorize [fg Cyan "----"]]
                            $ map (describe opts) matches
            where
                parser = mkGrepParser channel

data GrepOptions = GrepOptions {
        optContext  :: Int
    ,   optMatches  :: Int
    ,   optShowCmds :: Bool
    ,   optNick     :: Maybe Nick
    ,   optChannel  :: Channel
    ,   optQuery    :: T.Text
    }

mkGrepParser :: Channel -> Parser GrepOptions
mkGrepParser c = do
    optContext <- option auto (long "context"
               <> short 'c'
               <> metavar "CONTEXT"
               <> value 0
               <> help "The number of lines of context, on either side, of the match (default 0)")
    optMatches <- option auto (long "matches"
               <> short 'm'
               <> metavar "MATCHES"
               <> value 1
               <> help "The number of matches to return (default 1)")
    optShowCmds <- switch (long "show-commands"
                <> short 'S'
                <> help "Match lines prefixed with '!'")
    optNick <- optional (textOption (long "nick"
            <> short 'n'
            <> metavar "NICK"
            <> help "The nick to match against"))
    optChannel <- textOption (long "channel"
               <> short 'C'
               <> metavar "CHANNEL"
               <> value (T.unpack c)
               <> help "The channel to match against")
    optQuery <-  T.intercalate " "
             <$> (some . textArgument $ metavar "QUERY")

    pure GrepOptions{..}
    where
        textOption = fmap T.pack . strOption
        textArgument = fmap T.pack . strArgument

data GrepResult = GrepResult {
        resTime    :: UTCTime
    ,   resChannel :: Channel
    ,   resNick    :: Nick
    ,   resMessage :: [ColorText]
    ,   resPrefix  :: [(UTCTime, Nick, [ColorText])]
    ,   resSuffix  :: [(UTCTime, Nick, [ColorText])]
    }

parseGrepOptions :: Parser GrepOptions -> [T.Text] -> Maybe GrepOptions
parseGrepOptions parser = getParseResult
                        . execParserPure defaultPrefs (info parser fullDesc)
                        . fmap T.unpack

historyToList :: UTCTime -> Event -> [(UTCTime, Event)] -> [(UTCTime, Event)]
historyToList time ev@Shout{} acc = (time, ev) : acc
historyToList _    _          acc = acc

match :: GrepOptions
      -> [(UTCTime, Event)]
      -> [(UTCTime, Event)]
      -> [GrepResult]
match _ _ [] = []
match opts@GrepOptions{..} prefix (ev@(time, Shout evChannel evNick evMsg):evs)
    | optMatches <= 0                             = []
    | testMaybe optNick (/= evNick)               = skip
    | optChannel /= evChannel                     = skip
    | "!grep" `T.isPrefixOf` evMsg                = skip
    | not $ evMsg =~ optQuery                     = skip
    | "!" `T.isPrefixOf` evMsg && not optShowCmds = skip
    | otherwise                                   = isMatch
    where
        testMaybe = flip $ maybe False

        skip = match opts nextPrefix evs

        isMatch = result : match nextOpts nextPrefix evs

        nextPrefix = ev : prefix

        nextOpts = opts{optMatches = optMatches - 1}

        colorEvMessage = let (pre, msg, suf) = evMsg =~ optQuery
                          in [ colorText pre
                             , fg Red $ colorText msg
                             , colorText suf]

        result = GrepResult {
                resTime    = time
            ,   resChannel = evChannel
            ,   resNick    = evNick
            ,   resMessage = colorEvMessage
            ,   resPrefix  = colorContext $ reverse $ getContext prefix
            ,   resSuffix  = colorContext $ getContext evs
            }

        colorContext = map go
            where go (a, b, c) = (a, b, [colorText c])

        getContext = take optContext . map format . filter sameChannel

        sameChannel (_, Shout c _ _) = c == evChannel
        sameChannel _                = False

        format (t, Shout _ n m) = (t, n, m)
        format _                = error "Impossible non-shout"
match opts prefix (_:evs) = match opts prefix evs

describe :: GrepOptions -> GrepResult -> [T.Text]
describe GrepOptions{..} GrepResult{..} =
       map format resPrefix
    ++ format (resTime, resNick, resMessage)
    :  map format resSuffix
    where
    format (time, nick, msg) = colorize ([
            fromString $ show time
        ,   " ", colorText resChannel
        ,   " ", colorText nick
        ,   "> "
        ] ++ msg)

