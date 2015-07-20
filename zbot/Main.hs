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
import Zbot.Service.Help
import Zbot.Service.History
import Zbot.Service.Lists
import Zbot.Service.NGram
import Zbot.Service.Op
import Zbot.Service.Replace
import Zbot.Service.Reputation
import Zbot.Service.Roll
import Zbot.Service.Seen
import Zbot.Service.Summary
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
    registerService_ help
    registerService_ $ grep historyHandle
    registerService_ lists
    ngram historyHandle >>= registerService_
    registerService_ op
    registerService_ $ replace historyHandle
    registerService_ reputation
    registerService_ roll
    registerService_ $ seen historyHandle
    registerService_ $ summary historyHandle
    uptime >>= registerService_
