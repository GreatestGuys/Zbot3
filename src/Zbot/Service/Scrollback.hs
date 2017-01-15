{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Scrollback (
    scrollback
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
import Data.String (fromString)
import Data.Time.Clock (UTCTime)
import Safe (readMay)

import qualified Data.Text as T


data Options = Options {
    optChannel  :: Maybe Channel
,   optMessages :: Maybe Int
}

emptyOpts = Options Nothing Nothing

help :: HelpSpec
help = HelpSpec {
        helpAliases = ["!scrollback", "scrollback"]
    ,   helpMessage = [
        "usage: !scrollback [-C channel] -m messages"
    ,   ""
    ,   "   The scrollback service private messages the user a log of the most"
    ,   "recent public messages that have been sent on a given channel. If no"
    ,   "channel is specified then it will default to the current channel."
    ,   ""
    ,   "   It is an error to invoke !scrollback from a private message without"
    ,   "specifying a channel."
    ]
}

scrollback :: (MonadIO m, Bot m) => Handle m History -> Service m ()
scrollback historyHandle =
    (unitService "Zbot.Service.Scrollback" (handler historyHandle))
        {helpSpec = Just help}

handler :: (MonadIO m, Bot m)
        => Handle m History -> Event -> MonadService () m ()
handler hist (Whisper nick query)       = handleMsg hist nick emptyOpts query
handler hist (Shout channel nick query) = handleMsg hist nick
                                               (emptyOpts {
                                                    optChannel = Just channel
                                               })
                                               query
handler _      _                        = return ()

handleMsg :: (MonadIO m, Bot m)
          => Handle m History
          -> Nick
          -> Options
          -> T.Text
          -> MonadService () m ()
handleMsg hist nick initialOpts query
    | not ("!scrollback " `T.isPrefixOf` query) = return ()
    | opts <- parse (T.splitOn " " query) initialOpts
    , Options (Just channel)
              (Just messages) <- opts = getScroll hist reply channel messages
    | otherwise                       = lift $ reply "See !help !scrollback"
    where reply = whisper nick

parse :: [T.Text] -> Options -> Options
parse []   opts = opts
parse args opts
    | ("-m", Just _) <- (flag, intValue)  = parse rest
                                          $ opts {optMessages = intValue}
    | ("-C", _)      <- (flag, justValue) = parse rest
                                          $ opts {optChannel = justValue}
    | otherwise                           = parse (tail args) opts
    where
        (flag : flagRest) = args
        (value : rest)    = flagRest
        intValue = readMay $ T.unpack value
        justValue = Just value

getScroll :: (MonadIO m, Bot m)
          => Handle m History
          -> Reply m
          -> Channel
          -> Int
          -> MonadService () m ()
getScroll hist reply channel m =   lift
                               $   foldHistoryBackward hist search (m, [])
                               >>= mapM_ reply . snd
    where
        search :: UTCTime -> Event -> (Int, [T.Text]) -> (Int, [T.Text])
        search time (Shout ch nick msg) (n, xs)
            | n <= 0                       = (0, xs)
            | ch == channel                = (n - 1, format time nick msg : xs)
        search _    _                   a  = a

        format time nick msg = colorize [
                fromString $ show time
            ,   " ", colorText nick
            ,   "> "
            ,   colorText msg
            ]
