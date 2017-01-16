{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Dude (
    dude
)   where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Command
import Zbot.Extras.UnitService

import Control.Monad.Trans.Class (lift)

import qualified Data.Text as T


-- | The dude service never leaves a friend hanging.
dude :: Bot m => Service m ()
dude = unitService "Zbot.Service.Dude" (onCommand "o/" handler)

handler :: Bot m => Reply m -> T.Text -> MonadService () m ()
handler reply _ = lift $ reply "\\o"
