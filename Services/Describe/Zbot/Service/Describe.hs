module Zbot.Service.Describe (
    Describer
,   describe

,   scrapeURLAsDesktop
,   scrapeURLAsMobile
,   scrapeURLAsSearchEngine
,   scrapeURLAsUA
) where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Regex
import Zbot.Extras.UnitService

import Control.Applicative
import Network.Curl.Opts
import Network.Curl.Download
import Text.HTML.Scalpel
import Text.HTML.TagSoup

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T


-- | A Describer is a function that takes a message and optionally returns a
-- description of that message.
type Describer = T.Text -> Maybe (IO (Maybe T.Text))

describe :: (MonadIO m, Bot m) => [Describer] -> Service m ()
describe describers = unitService "Zbot.Service.Describe" handler
    where
        linkPattern = "https?://[^ ]*"

        handler = onRegex linkPattern describeLinks

        describeLinks reply link =
            maybe (return ()) ((replyMaybe =<<) . liftIO) maybeDescription
            where
                maybeDescription :: Maybe (IO (Maybe T.Text))
                maybeDescription = msum $ map ($ link) describers

                replyMaybe (Just description) = lift $ reply description
                replyMaybe _                  = return ()

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
