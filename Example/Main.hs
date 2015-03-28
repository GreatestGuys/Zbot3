module Main (main)
where

import Zbot.Cli
import Zbot.Core.Service
import Zbot.Service.Define
import Zbot.Service.Describe
import Zbot.Service.Describe.Default
import Zbot.Service.Describe.Onion
import Zbot.Service.Describe.Spotify
import Zbot.Service.Describe.Twitter
import Zbot.Service.Describe.YouTube
import Zbot.Service.Grep
import Zbot.Service.History
import Zbot.Service.Op
import Zbot.Service.Replace
import Zbot.Service.Roll
import Zbot.Service.Seen
import Zbot.Service.Uptime


main = zbotMain $ do
    historyHandle <- history >>= registerService

    registerService_ define
    registerService_ $ describe [
            describeOnion
        ,   describeSpotify
        ,   describeTwitter
        ,   describeYouTube
        ,   describeDefault
        ]
    registerService_ $ grep historyHandle
    registerService_ op
    registerService_ $ replace historyHandle
    registerService_ roll
    registerService_ $ seen historyHandle
    uptime >>= registerService_
