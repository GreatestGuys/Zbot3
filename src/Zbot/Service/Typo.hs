{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Typo (
    typo
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.UnitService
import Zbot.Service.History

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Data.List (minimumBy)
import Text.EditDistance (defaultEditCosts, levenshteinDistance)

import qualified Data.Text as T


-- | A service that lets a user address a typo in their previous message.
typo :: (MonadIO m, Bot m) => Int -> Handle m History -> Service m ()
typo maxDistance history = unitService "Zbot.Service.Typo" handler
    where
        handler (Shout channel nick msg)
            | "*" `T.isPrefixOf` msg && "*" `T.isSuffixOf` msg
            = return () -- Ignore things like "*vomit*".
            | "*" `T.isSuffixOf` msg
            = lift
            $ handleMessage (shout channel) channel nick msg
            | otherwise
            = return ()
        handler _ = return ()

        handleMessage reply channel nick msg = do
            lastMessage <- findLastMessage channel nick
            case lastMessage of
                Nothing            -> return ()
                (Just lastMessage) ->
                    case findTypo lastMessage (T.init msg) of
                        Nothing     -> return ()
                        (Just typo) -> reply
                                     . T.append nick
                                     . T.append ": "
                                     $ fixTypo lastMessage correction typo
            where
                correction = T.init msg -- drop the trailing "*"

        findLastMessage channel nick = foldHistoryBackward history match Nothing
            where
                match _ (Shout channel' nick' msg) Nothing
                    |  channel == channel'
                    && nick == nick'
                    && not ("*" `T.isSuffixOf` msg)
                    = Just msg
                match _ _ acc = acc

        findTypo lastMessage correction | editDistance bestMatch <= maxDistance
                                        = Just $ T.pack bestMatch
                                        | otherwise
                                        = Nothing

            where
                editDistance = levenshteinDistance defaultEditCosts
                             $ T.unpack correction
                compareWords w1 w2 = editDistance w1 `compare` editDistance w2
                bestMatch = minimumBy compareWords
                          . map T.unpack
                          $ T.words lastMessage

        fixTypo lastMessage correction typo = T.unwords
                                            . map (\word -> if word == typo
                                                then correction
                                                else word)
                                            $ T.words lastMessage
