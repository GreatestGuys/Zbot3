{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Describe.YouTube (
    describeYouTube
) where

import Zbot.Extras.Scrape
import Zbot.Service.Describe

import Text.HTML.Scalpel

import qualified Data.Text as T
import qualified Data.Text.Read as T


describeYouTube :: Describer
describeYouTube url
    | isYouTubeLink url = Just $ scrapeURLAsDesktop url info
    | otherwise         = Nothing

isYouTubeLink :: T.Text -> Bool
isYouTubeLink = (||) <$> ("youtube.com/watch" `T.isInfixOf`)
                     <*> ("youtu.be/" `T.isInfixOf`)

info :: Scraper T.Text T.Text
info = do
  title <- attr "content" $ "meta" @: ["property" @= "og:title"]
  -- The view count is given a div with the class "watch-view-count" and will be
  -- suffixed with the string " views".
  views <-  T.takeWhile (/= ' ')
        <$> text ("div" @: [hasClass "watch-view-count"])
  duration <-  prettyPrintDuration
           =<< attr "content" ("meta" @: ["itemprop" @= "duration"])
  return $ T.concat [
        title
    ,   " ["
    ,   "length: ", duration, ", "
    ,   "views: ", views
    ,   "]"
    ]

-- The duration is given in a weird format. Some examples:
--    - PT2M30S -> 2:30
--    - PT65M58S -> 01:05:59
prettyPrintDuration :: T.Text -> Scraper T.Text T.Text
prettyPrintDuration duration = do
  [minutes, seconds] <- return $ T.splitOn "M" $ T.drop 2 duration
  Right (minutes', _) <- return $ T.decimal minutes
  Right (seconds', _) <- return $ T.decimal seconds
  let secondText = formatText seconds'
  let minuteText = formatText $ minutes' `mod` 60
  let hourText = formatText $ minutes' `div` 60
  return $ T.intercalate ":" [hourText, minuteText, secondText]
  where
      formatText = T.justifyRight 2 '0' . T.pack . show
