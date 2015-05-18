module Zbot.Service.Seen (
    seen
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Command
import Zbot.Extras.Time
import Zbot.Extras.UnitService
import Zbot.Service.History

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Time.Clock

import qualified Data.Text as T


-- | A service that reports the time of a user's last activity.
seen :: (MonadIO m, Bot m) => Handle m History -> Service m ()
seen history = unitService "Zbot.Service.Seen" (onCommand "!seen" cmd)
    where
        cmd reply nick = lift $ do
            lastTime <- findLastTime nick
            now      <- liftIO getCurrentTime
            reply $ maybe notFound (found now) lastTime
            where
                notFound = T.concat ["There are no records of ", nick, "."]

                found now time = T.concat [
                        nick
                    ,   " was last active "
                    ,   prettyDiffTime (diffUTCTime now time)
                    ,   " ago."
                    ]

        findLastTime nick = foldHistoryBackward history match Nothing
            where
                match time (Shout _ nick' _) Nothing | nick == nick' = Just time
                match time (Whisper nick' _) Nothing | nick == nick' = Just time
                match _    _                 acc                     = acc
