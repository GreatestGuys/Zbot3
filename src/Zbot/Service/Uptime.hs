{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Uptime (
    uptime
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Message
import Zbot.Extras.Time
import Zbot.Extras.UnitService
import Zbot.Service.History

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Time.Clock

import qualified Data.Text as T


-- | A service that reports the bot's uptime.
uptime :: (MonadIO m, Bot m) => Handle m History -> m (Service m ())
uptime history = do
    bootTime <- liftIO getCurrentTime
    return $ unitService
        "Zbot.Service.Uptime"
        (onMessage $ uptimeHandler history bootTime)

uptimeHandler :: (MonadIO m, Bot m)
              => Handle m History
              -> UTCTime -> MessageContext m -> T.Text -> MonadService () m ()
uptimeHandler history bootTime ctx msg
    | ["!uptime"]       <- args = botUptime
    | ["!uptime", nick] <- args = nickUptime nick
    | otherwise      = return ()
    where
        args = T.words msg

        botUptime = do
            now <- liftIO getCurrentTime
            let up = diffUTCTime now bootTime
            lift $ reply ctx $ prettyDiffTime up

        nickUptime nick = lift $ do
            lastTime <- findJoinTime nick
            now      <- liftIO getCurrentTime
            reply ctx $ maybe notFound (found now) lastTime
            where
                notFound = T.concat ["There are no records of ", nick, "."]

                found now time = T.concat [
                        nick
                    ,   " joined "
                    ,   prettyDiffTime (diffUTCTime now time)
                    ,   " ago."
                    ]

        findJoinTime nick = foldHistoryBackward history match Nothing
            where
                match time (Join _ nick') Nothing | nick' == nick = Just time
                match _    _              acc                     = acc
