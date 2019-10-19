{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Zbot.Service.Version (
    version
)   where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Command
import Zbot.Extras.UnitService

import Control.Monad.Trans.Class (lift)
import Development.GitRev (gitCommitDate, gitHash)

import qualified Data.Text as T


-- | A service that prints the bot's most recent git hash.
version :: Bot m => Service m ()
version = unitService "Zbot.Service.Version" (onCommand "!version" handler)

handler :: Bot m => Reply m -> T.Text -> MonadService () m ()
handler reply _ = lift
                . reply Direct
                $ T.concat [
                    $(gitCommitDate)
                ,   " ("
                ,   $(gitHash)
                ,   ")"
                ]
