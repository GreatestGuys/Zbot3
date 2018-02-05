module Zbot.Service.Describe.Spotify (
    describeSpotify
) where

import Zbot.Extras.Scrape
import Zbot.Service.Describe

import Text.HTML.Scalpel

import qualified Data.Text as T


spotifyInfix = T.pack "open.spotify.com/"

describeSpotify :: Describer
describeSpotify url
    | spotifyInfix `T.isInfixOf` url = Just $ scrapeURLAsDesktop url info
    | otherwise                      = Nothing

info :: Scraper T.Text T.Text
info = do
    title <- text $ AnyTag @: [hasClass "primary-title"]
    artist <- text $ AnyTag @: [hasClass "secondary-title"]
    return $ T.concat [title, T.pack " by ", artist]
