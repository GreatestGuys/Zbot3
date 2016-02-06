{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Zbot.Service.Summary (
    summary
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Command
import Zbot.Extras.Regex
import Zbot.Extras.Scrape
import Zbot.Extras.UnitService
import Zbot.Service.History
import Zbot.Service.NGram.Model

import Control.Applicative ((<$>))
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Random.Sample (sample)
import Text.HTML.Scalpel

import qualified Data.Text as T


type NGramModel = Model 3 T.Text

-- | A service that will grant ops to users on the current channel.
summary :: (MonadIO m, Bot m) => Handle m History -> Service m ()
summary history = (unitService "Zbot.Service.Summary" handler) {
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
    where handler = onCommand "!summary" (handleCommand history)

handleCommand :: (MonadIO m, Bot m)
              => Handle m History -> Reply m -> T.Text -> MonadService () m ()
handleCommand history reply url
     | T.null url = lastSpokenUrl history >>= performSummary reply
     | otherwise  = performSummary reply url

performSummary :: (MonadIO m, Bot m)
               => Reply m -> T.Text -> MonadService () m ()
performSummary reply url = lift $ do
    maybeModel <- liftIO $ modelFromUrl url
    maybe replyError replyUttrance maybeModel
    where
        replyError = reply "That doesn't look like an article to me."
        replyUttrance model =   liftIO (sample $ detokenize <$> generate model)
                            >>= reply

lastSpokenUrl :: (MonadIO m, Bot m)
              => Handle m History -> MonadService () m T.Text
lastSpokenUrl history = lift $ foldHistoryBackward history findUrl ""
    where
        findUrl _ _               url | not (T.null url) = url
        findUrl _ (Shout _ _ msg) _                      = extractUrl msg
        findUrl _ (Whisper _ msg) _                      = extractUrl msg
        findUrl _ _               _                      = ""

        extractUrl = fromMaybe "" . extractRegex "https?://[^ #]*"

modelFromUrl :: T.Text -> IO (Maybe NGramModel)
modelFromUrl url = runMaybeT $ do
    sentences <- liftIO $ scrapeArticleSentences url
    guard (not $ null sentences)
    return $ makeModel sentences

scrapeArticleSentences :: T.Text -> IO [[T.Text]]
scrapeArticleSentences url = do
    maybeParagraphs <- scrapeURLAsDesktop url $ texts paragraphScraper
    let maybeSentences = concatMap toSentences <$> maybeParagraphs
    return $ maybe [] (map tokenize) maybeSentences
    where paragraphScraper = ("article" :: String) // ("p" :: String)

stripNonAscii :: T.Text -> T.Text
stripNonAscii = T.map (\x -> if ord x >= 32 && ord x < 126 then x else ' ')

toSentences :: T.Text -> [T.Text]
toSentences = map (`T.append` ".")
            . filter (not . T.null)
            . T.split (== '.')
            . stripNonAscii

tokenize :: T.Text -> [T.Text]
tokenize = T.words

detokenize :: [T.Text] -> T.Text
detokenize = T.unwords
