{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Zbot.Service.Grep (
    grep
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Color
import Zbot.Extras.Command
import Zbot.Extras.UnitService
import Zbot.Service.History

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Data.List (intercalate)
import Data.String (fromString)
import Data.Time.Clock (UTCTime)
import Safe (readMay)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

import qualified Data.Text as T


grep :: (MonadIO m, Bot m) => Handle m History -> Service m ()
grep historyHandle =
    (unitService "Zbot.Service.Grep" (onCommand "!grep" grepCommand)) {
        helpSpec = Just HelpSpec {
                helpAliases = ["!grep"]
            ,   helpMessage = [T.intercalate " " [
                        "usage: !grep"
                    ,   "[-c context]"
                    ,   "[-C channel]"
                    ,   "[-n nick]"
                    ,   "[-m matches]"
                    ,   "regex"
                    ]]
            }
    }
    where
        grepCommand reply args = lift $ do
            let opts = parse args defaultOptions
            history <-  drop 1
                    <$> foldHistoryForward historyHandle historyToList []
            let matches = match opts [] history
            mapM_ reply $ reverse
                        $ intercalate [colorize [fg Cyan "----"]]
                        $ map (describe opts) matches

data GrepOptions = GrepOptions {
        optContext :: Int
    ,   optMatches :: Int
    ,   optNick    :: Maybe Nick
    ,   optChannel :: Maybe Channel
    ,   optQuery   :: T.Text
    }

data GrepResult = GrepResult {
        resTime    :: UTCTime
    ,   resChannel :: Channel
    ,   resNick    :: Nick
    ,   resMessage :: [ColorText]
    ,   resPrefix  :: [(UTCTime, Nick, [ColorText])]
    ,   resSuffix  :: [(UTCTime, Nick, [ColorText])]
    }

defaultOptions :: GrepOptions
defaultOptions = GrepOptions {
        optContext = 0
    ,   optMatches = 1
    ,   optNick    = Nothing
    ,   optChannel = Nothing
    ,   optQuery   = "."
    }

parse :: T.Text -> GrepOptions -> GrepOptions
parse args opts
    | ("-m", Just m) <- (flag, intValue)  = parse rest $ opts {optMatches = m}
    | ("-c", Just c) <- (flag, intValue)  = parse rest $ opts {optContext = c}
    | ("-C", c)      <- (flag, justValue) = parse rest $ opts {optChannel = c}
    | ("-n", n)      <- (flag, justValue) = parse rest $ opts {optNick = n}
    | T.null args                         = opts {optQuery = "."}
    | otherwise                           = opts {optQuery = args}
    where
        (flag, flagRest) = T.breakOn " " args
        (value, valueRest) = T.breakOn " "  $ T.stripStart flagRest
        rest = T.stripStart valueRest
        intValue = readMay $ T.unpack value
        justValue = Just value

historyToList :: UTCTime -> Event -> [(UTCTime, Event)] -> [(UTCTime, Event)]
historyToList time ev@Shout{} acc = (time, ev) : acc
historyToList _    _          acc = acc

match :: GrepOptions
      -> [(UTCTime, Event)]
      -> [(UTCTime, Event)]
      -> [GrepResult]
match _ _ [] = []
match opts@GrepOptions{..} prefix (ev@(time, Shout evChannel evNick evMsg):evs)
    | optMatches <= 0                     = []
    | testMaybe optNick (/= evNick)       = skip
    | testMaybe optChannel (/= evChannel) = skip
    | "!grep" `T.isPrefixOf` evMsg        = skip
    | not $ evMsg =~ optQuery             = skip
    | otherwise                           = isMatch
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

