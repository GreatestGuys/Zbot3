module Zbot.Service.Describe.Default (
    describeDefault
) where

import Zbot.Service.Describe

import Text.HTML.Scalpel

import qualified Data.Text as T

describeDefault :: Describer
describeDefault url= Just $ scrapeURLAsSearchEngine url title

title :: Scraper T.Text T.Text
title = text $ ("title" :: T.Text)
