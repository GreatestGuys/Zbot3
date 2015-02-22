module Main (main)
where

import Zbot.Cli
import Zbot.Core.Service
import Zbot.Service.Describe
import Zbot.Service.Describe.Default
import Zbot.Service.Describe.Onion
import Zbot.Service.Describe.Spotify
import Zbot.Service.Describe.Twitter
import Zbot.Service.Describe.YouTube
import Zbot.Service.Roll

main = zbotMain $ do
    registerService_ roll
    registerService_ $ describe [
            describeOnion
        ,   describeSpotify
        ,   describeTwitter
        ,   describeYouTube
        ,   describeDefault
        ]
