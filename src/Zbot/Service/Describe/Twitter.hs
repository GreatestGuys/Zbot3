module Zbot.Service.Describe.Twitter (
    describeTwitter
) where

import Zbot.Extras.Scrape
import Zbot.Service.Describe

import Control.Applicative ((<$>))
import Text.HTML.Scalpel

import qualified Data.Text as T


twitterInfixes = map T.pack ["twitter.com/", "/status/"]

infixesOf :: [T.Text] -> T.Text -> Bool
infixesOf infixes target = all (`T.isInfixOf` target) infixes

describeTwitter :: Describer
describeTwitter url
    | twitterInfixes `infixesOf` url = Just $ scrapeURLAsSearchEngine url tweet
    | otherwise                      = Nothing

tweet :: Scraper T.Text T.Text
tweet = do
    status   <-  extractStatus
             <$> attr "content" ("meta" @: ["property" @= "og:description"])
    userName <-  extractUserName
             <$> attr "content" ("meta" @: ["property" @= "og:url"])
    fullName <-  extractFullName
             <$> attr "content" ("meta" @: ["property" @= "og:title"])
    return $ T.concat [userName, T.pack " (", fullName, T.pack ") ", status]

dropFromEnd :: Int -> T.Text -> T.Text
dropFromEnd i text = T.take (T.length text - i) text

extractStatus :: T.Text -> T.Text
extractStatus = dropFromEnd 1 . T.drop 1

extractUserName :: T.Text -> T.Text
extractUserName url
    | (_:url':_) <- T.splitOn (T.pack ".com/") url
    , (user:_)   <- T.splitOn (T.pack "/") url'    = user
    | otherwise                                    = T.empty

extractFullName :: T.Text -> T.Text
extractFullName = dropFromEnd (length " on Twitter")
