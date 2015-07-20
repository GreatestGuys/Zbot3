module Zbot.Extras.UnitService (
    unitService
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service

import qualified Data.Text as T


unitService :: Bot m => T.Text -> (Event -> MonadService () m ()) -> Service m ()
unitService name process = Service {
        initial     = ()
    ,   serialize   = const Nothing
    ,   deserialize = const Nothing
    ,   name        = name
    ,   process     = process
    ,   helpSpec    = Nothing
    }
