module Zbot.Service.Describe.Default (
    describeDefault
) where

import Zbot.Extras.Scrape
import Zbot.Service.Describe

import Control.Applicative
import Text.HTML.Scalpel

import qualified Data.Text as T


describeDefault :: Describer
describeDefault url= Just $ scrapeURLAsSearchEngine url description

description :: Scraper T.Text T.Text
description = metaDescription
            <|> metaFacebookDescription
            <|> metaTwitterDescription
            <|> title

scrapeMetaTag :: T.Text -> Scraper T.Text T.Text
scrapeMetaTag name = do
    content <- attr "content"
            $ ("meta" :: T.Text) @: [("name" :: T.Text) @= name]
    if T.null content then empty else return content

metaDescription :: Scraper T.Text T.Text
metaDescription = scrapeMetaTag "description"

metaFacebookDescription :: Scraper T.Text T.Text
metaFacebookDescription = scrapeMetaTag "og:description"

metaTwitterDescription :: Scraper T.Text T.Text
metaTwitterDescription = scrapeMetaTag "twitter:description"

title :: Scraper T.Text T.Text
title = text ("title" :: T.Text)
