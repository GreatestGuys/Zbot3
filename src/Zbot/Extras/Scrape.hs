module Zbot.Extras.Scrape (
    scrapeURLAsDesktop
,   scrapeURLAsMobile
,   scrapeURLAsSearchEngine
,   scrapeURLAsUA
) where

import Network.Curl.Opts (CurlOption (..))
import Text.HTML.Scalpel

import qualified Data.Text as T


scrapeURLAsSearchEngine :: T.Text -> Scraper T.Text a -> IO (Maybe a)
scrapeURLAsSearchEngine = scrapeURLAsUA "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)"

scrapeURLAsMobile :: T.Text -> Scraper T.Text a -> IO (Maybe a)
scrapeURLAsMobile = scrapeURLAsUA "NOKIA"

scrapeURLAsDesktop :: T.Text -> Scraper T.Text a -> IO (Maybe a)
scrapeURLAsDesktop = scrapeURLAsUA "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

scrapeURLAsUA :: String -> T.Text -> Scraper T.Text a -> IO (Maybe a)
scrapeURLAsUA userAgent textUrl = scrapeURLWithOpts options url
    where
        url = T.unpack textUrl
        options = [
                CurlUserAgent userAgent
            ,   CurlFollowLocation True
            ]
