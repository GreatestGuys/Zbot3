module Zbot.Service.Describe.Twitter (
    describeTwitter
) where

import Zbot.Extras.Scrape
import Zbot.Service.Describe

import Text.HTML.Scalpel

import qualified Data.Text as T


twitterInfixes = ["twitter.com/", "/status/"]

infixesOf :: [T.Text] -> T.Text -> Bool
infixesOf infixes target = all (`T.isInfixOf` target) infixes

describeTwitter :: Describer
describeTwitter url
    | twitterInfixes `infixesOf` url = Just $ scrapeURLAsSearchEngine url tweet
    | otherwise                      = Nothing

tweet :: Scraper T.Text T.Text
tweet = chroot (Any @: [hasClass "tweet"]) $ do
    status   <- text $ Any @: [hasClass "tweet-text"]
    userName <- text $ Any @: [hasClass "username"]
    fullName <- text $ Any @: [hasClass "fullname"]
    return $ T.concat [userName, " (", fullName, ") ", status]
