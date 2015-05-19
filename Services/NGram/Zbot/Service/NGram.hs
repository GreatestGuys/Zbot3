module Zbot.Service.NGram (
    ngram
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.UnitService
import Zbot.Service.History

import Control.Monad.Trans.Class (lift)

import qualified Data.Text as T


ngram :: Bot m => Handle m History -> Service m ()
ngram history = unitService "Zbot.Service.NGram" (handler history)

handler :: Bot m => Handle m History -> Event -> MonadService () m ()
handler history event = return ()
