{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Zbot.Extras.Scrape (
    scrapeURLAsDesktop
,   scrapeURLAsMobile
,   scrapeURLAsSearchEngine
,   scrapeURLAsUA
) where

import Data.Default (def)
import Text.HTML.Scalpel

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP


scrapeURLAsSearchEngine :: T.Text -> Scraper T.Text a -> IO (Maybe a)
scrapeURLAsSearchEngine = scrapeURLAsUA "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)"

scrapeURLAsMobile :: T.Text -> Scraper T.Text a -> IO (Maybe a)
scrapeURLAsMobile = scrapeURLAsUA "NOKIA"

scrapeURLAsDesktop :: T.Text -> Scraper T.Text a -> IO (Maybe a)
scrapeURLAsDesktop = scrapeURLAsUA "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

scrapeURLAsUA :: BS.ByteString -> T.Text -> Scraper T.Text a -> IO (Maybe a)
scrapeURLAsUA userAgent textUrl scraper = do
  manager <- Just <$> HTTP.newManager managerSettings
  scrapeURLWithConfig (def { manager }) url scraper
  where
    url = T.unpack textUrl

    managerSettings :: HTTP.ManagerSettings
    managerSettings = HTTP.tlsManagerSettings {
      HTTP.managerModifyRequest = \req -> do
        req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
        return $ req' {
            HTTP.requestHeaders = (HTTP.hUserAgent, userAgent)
                                : HTTP.requestHeaders req'
          },

      HTTP.managerModifyResponse = \res -> do
        res' <- HTTP.managerModifyResponse HTTP.tlsManagerSettings res
        let isTextContent = (not . null)
                          $ filter ("text/" `BS.isInfixOf`)
                          $ map snd
                          $ filter isContentType
                          $ HTTP.responseHeaders res'
        -- For all non-text requests return an empty body.
        if isTextContent
          then return res'
          else return $ res' {
              HTTP.responseBody = return BS.empty
            }
    }

    isContentType = (== HTTP.hContentType) . fst
