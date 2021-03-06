{-# LANGUAGE NoOverloadedStrings #-}
module Zbot.Service.Describe.Onion (
    describeOnion
) where

import Zbot.Extras.Scrape
import Zbot.Service.Describe

import Text.HTML.Scalpel

import qualified Data.Text as T


onionInfix = T.pack "theonion.com"

describeOnion :: Describer
describeOnion url
    | onionInfix `T.isInfixOf` url = Just $ scrapeURLAsSearchEngine url title
    | otherwise                    = Nothing

title :: Scraper T.Text T.Text
title = do
    fullTitle <- text $ tagSelector "title"
    let shortTitle = T.strip $ T.takeWhile ('-' /=) fullTitle
    return $ T.pack "Breaking News: " `T.append` shortTitle
