{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.NGram (
    ngram
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.UnitService
import Zbot.Extras.Command
import Zbot.Service.History
import Zbot.Service.NGram.Model

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (mapMaybe)
import Data.Monoid (mconcat)
import Data.Random.Sample (sample)
import Data.Time.Clock (UTCTime)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T


type NGramModel = Model 3 T.Text

ngram :: (MonadIO m, Bot m) => Handle m History -> m (Service m ())
ngram history = do
    models <- buildNGramModels history
    return $
      (unitService "Zbot.Service.NGram" (onCommand "!be" $ handler models)) {
            helpSpec    = Just HelpSpec {
                helpAliases      = ["!be"]
            ,   helpMessage      = ["usage: !be nick"]
            }
        }

handler :: (MonadIO m, Bot m)
        => Map.Map Nick NGramModel -> Reply m -> T.Text -> MonadService () m ()
handler modelMap reply args | [] <- models = lift
                                           $ reply "usage: !be nick1 nick2 ..."
                            | otherwise    =   liftIO (sample utterance)
                                           >>= lift . reply
    where
        nicks  = T.words args
        models = mapMaybe (`Map.lookup` modelMap) nicks
        combinedModel = mconcat models
        utterance = detokenize <$> generate combinedModel

buildNGramModels :: (MonadIO m, Bot m)
                 => Handle m History -> m (Map.Map Nick NGramModel)
buildNGramModels history =   fmap makeModel
                         <$> foldHistoryForward history catMessages Map.empty

catMessages :: UTCTime
            -> Event
            -> Map.Map Nick [[T.Text]]
            -> Map.Map Nick [[T.Text]]
catMessages _ (Shout _ nick message) nickMap =
        -- This ++ isn't as bad as it looks since the first argument is going to
        -- be the singleton list [tokenize message] the time to traverse this
        -- list is O(1).
        Map.insertWith (++) nick [tokenize message] nickMap
catMessages _ _                      nickMap = nickMap

tokenize :: T.Text -> [T.Text]
tokenize = T.words

detokenize :: [T.Text] -> T.Text
detokenize = T.unwords
