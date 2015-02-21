module Zbot.Extras.UnitService (
    unitService
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service

import Control.Monad.State

import qualified Data.Text as T

unitService :: Bot m => T.Text -> (Event -> StateT () m ()) -> Service m ()
unitService name process = Service {
        initial     = ()
    ,   serialize   = const Nothing
    ,   deserialize = const Nothing
    ,   name        = name
    ,   process     = process
    }
