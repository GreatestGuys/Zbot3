{-# LANGUAGE DataKinds #-}
module Zbot.Service.Summary (
    summary
)   where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Command
import Zbot.Extras.Scrape
import Zbot.Extras.UnitService
import Zbot.Service.NGram.Model

import Control.Applicative ((<$>))
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Char (ord)
import Data.Random.Sample (sample)
import Text.HTML.Scalpel

import qualified Data.Text as T


type NGramModel = Model 3 T.Text

-- | A service that will grant ops to users on the current channel.
summary :: (MonadIO m, Bot m) => Service m ()
summary = (unitService "Zbot.Service.Summary" (onCommand "!summary" handler)) {
        helpSpec = Just HelpSpec {
                helpAliases = ["!summary"]
            ,   helpMessage = [
                    "usage: !summary [url]"
                ,   ""
                ,   "  Summarizes a given URL by training an n-gram model on"
                ,   "the contents of the article and replying with a random"
                ,   "utterance. If no URL is given, then the last mentioned"
                ,   "HTTP/S link is used as the input."
                ]
            }
    }

handler :: (MonadIO m, Bot m) => Reply m -> T.Text -> MonadService () m ()
handler reply url = do
    maybeModel <- liftIO $ modelFromUrl url
    lift $ maybe replyError replyUttrance maybeModel
    where
        replyError = reply "Couldn't make heads or tails of it, sorry :("
        replyUttrance model =   liftIO (sample $ detokenize <$> generate model)
                            >>= reply

modelFromUrl :: T.Text -> IO (Maybe NGramModel)
modelFromUrl url = runMaybeT $ do
    sentences <- liftIO $ scrapeArticleSentences url
    guard (not $ null sentences)
    return $ makeModel sentences

scrapeArticleSentences :: T.Text -> IO [[T.Text]]
scrapeArticleSentences url = do
    maybeParagraphs <- scrapeURLAsDesktop url $ texts paragraphScraper
    let maybeSentences = concatMap toSentences <$> maybeParagraphs
    return $ maybe [] (map tokenize. filter (not . T.null)) maybeSentences
    where paragraphScraper = ("article" :: String) // ("p" :: String)

stripNonAscii :: T.Text -> T.Text
stripNonAscii = T.map (\x -> if ord x >= 32 && ord x < 126 then x else ' ')

toSentences :: T.Text -> [T.Text]
toSentences = map (`T.append` ".") . filter (not . T.null) . T.split (== '.') . stripNonAscii

tokenize :: T.Text -> [T.Text]
tokenize = T.words

detokenize :: [T.Text] -> T.Text
detokenize = T.unwords
