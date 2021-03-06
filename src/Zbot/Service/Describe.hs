{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Describe (
    Describer
,   describe
) where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Regex
import Zbot.Extras.UnitService

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)

import qualified Data.Text as T


-- | A Describer is a function that takes a message and optionally returns a
-- description of that message.
type Describer = T.Text -> Maybe (IO (Maybe T.Text))

describe :: (MonadIO m, Bot m) => [Describer] -> Service m ()
describe describers = unitService "Zbot.Service.Describe" handler'
    where
        linkPattern = "https?://[^ #]*"

        handler = onRegex linkPattern describeLinks

        handler' (Shout _ _ msg) | "!" `T.isPrefixOf` msg = return ()
        handler' (Whisper _ msg) | "!" `T.isPrefixOf` msg = return ()
        handler' event                                    = handler event

        describeLinks ctx link =
            maybe (return ()) ((replyMaybe =<<) . liftIO) maybeDescription
            where
                maybeDescription :: Maybe (IO (Maybe T.Text))
                maybeDescription = msum $ map ($ link) describers

                replyMaybe (Just description) = lift $ reply ctx description
                replyMaybe _                  = return ()
