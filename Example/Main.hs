module Main (main)
where

import Zbot.Cli
import Zbot.Core.Service
import Zbot.Service.Roll

main = zbotMain $ do
    registerService_ roll
