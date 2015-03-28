module Zbot.Service.Uptime (
    uptime
)   where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Command
import Zbot.Extras.Message
import Zbot.Extras.Time
import Zbot.Extras.UnitService

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Time.Clock

import qualified Data.Text as T


-- | A service that reports the bot's uptime.
uptime :: (MonadIO io, MonadIO m, Bot m) => io (Service m ())
uptime = do
    bootTime <- liftIO getCurrentTime
    return $ unitService
        "Zbot.Service.Uptime"
        (onCommand "!uptime" (handler bootTime))

handler :: (MonadIO m, Bot m)
        => UTCTime -> Reply m -> T.Text -> MonadService () m ()
handler bootTime reply _ = do
    now <- liftIO getCurrentTime
    let up = diffUTCTime now bootTime
    lift $ reply $ prettyDiffTime up
