module Zbot.Extras.Scrape (
    scrapeURLAsDesktop
,   scrapeURLAsMobile
,   scrapeURLAsSearchEngine
,   scrapeURLAsUA
) where

import Control.Applicative
import Network.Curl.Opts
import Network.Curl.Download
import Text.HTML.Scalpel
import Text.HTML.TagSoup

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

scrapeURLAsSearchEngine :: T.Text -> Scraper T.Text a -> IO (Maybe a)
scrapeURLAsSearchEngine = scrapeURLAsUA "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)"

scrapeURLAsMobile :: T.Text -> Scraper T.Text a -> IO (Maybe a)
scrapeURLAsMobile = scrapeURLAsUA "NOKIA"

scrapeURLAsDesktop :: T.Text -> Scraper T.Text a -> IO (Maybe a)
scrapeURLAsDesktop = scrapeURLAsUA "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

scrapeURLAsUA :: String -> T.Text -> Scraper T.Text a -> IO (Maybe a)
scrapeURLAsUA userAgent textUrl scraper = do
        maybeTags <- downloadAsTags url
        return (maybeTags >>= scrape scraper)
    where
        url = T.unpack textUrl
        options = [
                CurlUserAgent userAgent
            ,   CurlFollowLocation True
            ]
        downloadAsTags url = do
            bs <- maybeRight <$> openURIWithOpts options url
            return $ parseTags <$> (bs >>= maybeRight . T.decodeUtf8')
        maybeRight (Right a) = Just a
        maybeRight _         = Nothing
