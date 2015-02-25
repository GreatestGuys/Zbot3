module Zbot.Service.Describe.Spotify (
    describeSpotify
) where

import Zbot.Service.Describe

import Text.HTML.Scalpel

import qualified Data.Text as T


spotifyInfix = "open.spotify.com/"

describeSpotify :: Describer
describeSpotify url
    | spotifyInfix `T.isInfixOf` url = Just $ scrapeURLAsDesktop url info
    | otherwise                      = Nothing

info :: Scraper T.Text T.Text
info = do
    title <- text $ Any @: [hasClass "primary-title"]
    artist <- text $ Any @: [hasClass "secondary-title"]
    return $ T.concat [title, " by ", artist]
