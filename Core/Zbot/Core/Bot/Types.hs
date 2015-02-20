module Zbot.Core.Bot.Types (
    Bot
) where

import Zbot.Core.Irc
import Zbot.Core.Service


class (Collective m, Irc m) => Bot m where
