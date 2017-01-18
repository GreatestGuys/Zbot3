{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Replace (
    replace
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.UnitService
import Zbot.Service.History

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Data.Monoid

import qualified Data.Text as T


-- | A service that will replace a substring of a users previous message.
replace :: (MonadIO m, Bot m) => Handle m History -> Service m ()
replace history = unitService "Zbot.Service.Replace" handler
    where
        handler (Shout channel nick msg) = lift $ do
            maybeLine <- findLastMessage channel
            handleMessage (shout channel) nick maybeLine msg
        handler _                        = return ()

        handleMessage reply nick (Just line) msg
            | ("s":from:to:"g":_) <- split msg = handleReplace $ replaceAll from to line
            | ("s":from:to:_)     <- split msg = handleReplace $ replaceOne from to line
            where
                handleReplace rline = if rline == line
                    then return ()
                    else reply $ nick `T.append` ": " `T.append` rline
        handleMessage _ _ _ _                    = return ()

        findLastMessage channel = foldHistoryBackward history match Nothing
            where
                match _ (Shout channel' _ msg) Nothing
                    | channel == channel'
                      && not ("s/" `T.isPrefixOf` msg) = Just msg
                match _ _ acc = acc

-- | Split a string on all non-escaped back slashes.
split :: T.Text -> [T.Text]
split = splitHelper ""
    where
        splitHelper acc s
            | T.null s               = [acc]
            | "\\/" `T.isPrefixOf` s = splitHelper (acc <> "\\/") (T.drop 2 s)
            | "/" `T.isPrefixOf` s   = acc : splitHelper "" (T.drop 1 s)
            | otherwise              = splitHelper
                                            (acc <> T.take 1 s)
                                            (T.drop 1 s)

replaceAll :: T.Text -> T.Text -> T.Text -> T.Text
replaceAll from to t
    | T.null from = t
    | otherwise   = T.replace from to t

replaceOne :: T.Text -> T.Text -> T.Text -> T.Text
replaceOne from to t
    | T.null from || T.null t = t
    | from `T.isPrefixOf` t   = to <> T.drop (T.length from) t
    | otherwise               = T.take 1 t <> replaceOne from to (T.drop 1 t)
