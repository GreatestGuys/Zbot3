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
import Zbot.Service.Dude
import Zbot.Service.GitHub
import Zbot.Service.Goblin
import Zbot.Service.Grep
import Zbot.Service.Help
import Zbot.Service.History
import Zbot.Service.Invite
import Zbot.Service.Lists
import Zbot.Service.Morse
import Zbot.Service.NGram
import Zbot.Service.Op
import Zbot.Service.Remind
import Zbot.Service.Replace
import Zbot.Service.Reputation
import Zbot.Service.Roll
import Zbot.Service.Scrollback
import Zbot.Service.Seen
import Zbot.Service.Summary
import Zbot.Service.Uptime
import Zbot.Service.Version


main = zbotMain $ do
    historyHandle <- history >>= registerService

    registerService_ acceptInvite
    registerService_ define
    registerService_ $ describe [
            describeOnion
        ,   describeSpotify
        ,   describeTwitter
        ,   describeYouTube
        ,   describeDefault
        ]
    registerService_ dude
    registerService_ github
    registerService_ goblin
    registerService_ $ grep historyHandle
    registerService_ help
    registerService_ lists
    registerService_ morse
    ngram historyHandle >>= registerService_
    registerService_ op
    registerService_ remind
    registerService_ $ replace historyHandle
    registerService_ reputation
    registerService_ roll
    registerService_ $ scrollback historyHandle
    registerService_ $ seen historyHandle
    registerService_ $ summary historyHandle
    uptime historyHandle >>= registerService_
    registerService_ version
