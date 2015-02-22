module Zbot.Service.Describe.Spotify (
    describeSpotify
) where

import Zbot.Service.Describe

import Text.HTML.Scalpel

import qualified Data.Text as T

spotifyInfix = "open.spotify.com/"

describeSpotify :: Describer
describeSpotify url
    | spotifyInfix `T.isInfixOf` url = Just $ scrapeURLAsSearchEngine url info
    | otherwise                      = Nothing

info :: Scraper T.Text T.Text
info = do
    fullTitle <- text $ ("title" :: T.Text)
    -- Drop the " on Spotify" from end of the title.
    return $ T.take (T.length fullTitle - 11) fullTitle
