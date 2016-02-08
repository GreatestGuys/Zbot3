{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.History (
    history
,   History

,   foldHistoryBackward
,   foldHistoryForward
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Service.History.Log

import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock (UTCTime)

import qualified Data.Text as T


newtype History = MkHistory ()

-- | A service that will keep track of all IRC events that Zbot ever receives.
history :: (MonadIO io, MonadIO m, Bot m) => io (Service m History)
history =
    return Service {
            initial     = MkHistory ()
        ,   serialize   = const Nothing
        ,   deserialize = const Nothing
        ,   name        = "Zbot.Service.History"
        ,   process     = handleEvent
        ,   helpSpec    = Nothing
        }

logFileName :: T.Text
logFileName = "log"

handleEvent :: (MonadIO m, Bot m) => Event -> MonadService History m ()
handleEvent event = do
    logPath <- sandboxedFilePath logFileName
    appendLog logPath event

foldHistoryBackward :: (MonadIO m, Collective m)
                    => Handle m History
                    -> (UTCTime -> Event -> a -> a) -> a -> m a
foldHistoryBackward handle f initial = run handle $ do
    logPath <- sandboxedFilePath logFileName
    foldLogBackward f initial logPath

foldHistoryForward :: (MonadIO m, Collective m)
                   => Handle m History
                   -> (UTCTime -> Event -> a -> a) -> a -> m a
foldHistoryForward handle f initial = run handle $ do
    logPath <- sandboxedFilePath logFileName
    foldLogForward f initial logPath
