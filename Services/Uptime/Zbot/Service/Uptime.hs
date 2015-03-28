module Zbot.Service.Uptime (
    uptime
)   where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Message
import Zbot.Extras.Command
import Zbot.Extras.UnitService

import Control.Arrow (first)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.List (foldl')
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
    lift $ reply $ T.pack $ pretty up

pretty :: NominalDiffTime -> String
pretty td   =   unwords
            $   map (uncurry (++) . first show)
            $   if null diffs
                    then [(0, "s")]
                    else diffs
    where
        merge (tot,acc) (sec,typ) =
            let (sec', tot') = divMod tot sec
            in (tot', (sec', typ):acc)

        metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]

        diffs   = filter ((/=0) . fst)
                $ reverse
                $ snd
                $ foldl' merge (round td, []) metrics
