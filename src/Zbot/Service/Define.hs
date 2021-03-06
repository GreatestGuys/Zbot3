{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Define (
    define
) where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Command
import Zbot.Extras.Scrape
import Zbot.Extras.UnitService

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad (guard)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Text.HTML.Scalpel

import qualified Data.Text as T
import qualified Network.HTTP as HTTP


define :: (MonadIO m, Bot m) => Service m ()
define = (unitService "Zbot.Service.Define" (onCommand "!define" lookupTerm)) {
        helpSpec = Just HelpSpec {
                helpAliases = ["!define"]
            ,   helpMessage = ["usage: !define word"]
            }
    }

lookupTerm :: (MonadIO m, Bot m) => MessageContext m -> T.Text -> MonadService s m ()
lookupTerm ctx term = do
    maybeDef <- liftIO $ tryIOMaybes [scrapeTerm term, scrapeRandom term]
    lift $ maybe (return ()) (reply ctx) maybeDef

tryIOMaybes :: [IO (Maybe a)] -> IO (Maybe a)
tryIOMaybes []     = return Nothing
tryIOMaybes (x:xs) = x >>= maybe (tryIOMaybes xs) (return . Just)

scrapeTerm :: T.Text -> IO (Maybe T.Text)
scrapeTerm term = runMaybeT $ do
    let encodedTerm = HTTP.urlEncode $ T.unpack term
    let url = T.pack
            $ "http://www.urbandictionary.com/define.php?term=" ++ encodedTerm
    defn <- MaybeT $ scrapeURLAsDesktop url meaning
    guard $ not ("There aren't any definitions" `T.isInfixOf` defn)
    return $ format term defn

scrapeRandom :: T.Text -> IO (Maybe T.Text)
scrapeRandom term = do
    let url = "http://www.urbandictionary.com/random.php"
    replaceAndFormat <$> scrapeURLAsDesktop url ((,) <$> meaning <*> header)
    where
        replaceAndFormat Nothing                  = Nothing
        replaceAndFormat (Just (def, randomTerm)) =
                Just $ format term (replace randomTerm term def)

meaning :: Scraper T.Text T.Text
meaning = text $ AnyTag @: [hasClass "meaning"]

header :: Scraper T.Text T.Text
header = text $ AnyTag @: [hasClass "word"]

format :: T.Text -> T.Text -> T.Text
format word def = T.concat [word, ": ", def']
    where
        def' = T.replace "\n" " "
             $ T.replace "\r" ""
             $ T.strip def

-- Performs a case insensitive replacement.
replace :: T.Text -> T.Text -> T.Text -> T.Text
replace old new source = pack $ r (unpack old) (unpack new) (unpack source)
    where
        unpack = T.unpack
        pack   = T.pack

        r _   _   []      = []
        r []  _   source  = source

        r old new source
            | map toLower old `isPrefixOf` map toLower source
            = new ++ r old new (drop (length old) source)

        r old new (x:xs)  = x : r old new xs
