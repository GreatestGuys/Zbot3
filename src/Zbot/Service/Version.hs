{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Version (
    version
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.UnitService

import Control.Monad.Trans.Class (lift)


-- | A service that prints the bot's most recent git hash.
version :: Bot m => Service m ()
version = unitService "Zbot.Service.Version" handler

handler :: Bot m => Event -> MonadService () m ()
handler (Shout channel _ "!version") = lift $ do
    version <- myVersion
    shout channel version
handler _                            = return ()
