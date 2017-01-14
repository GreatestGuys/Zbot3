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
            maybeLine <- findLastMessage channel nick
            handleMessage (shout channel) maybeLine msg
        handler _                        = return ()

        handleMessage reply (Just line) msg
            | ("s":from:to:"g":_) <- split msg = handleReplace $ replaceAll from to line
            | ("s":from:to:_)     <- split msg = handleReplace $ replaceOne from to line
            where
                handleReplace rline = if rline == line
                    then reply rline
                    else return ()
        handleMessage _ _ _                    = return ()

        findLastMessage channel nick = foldHistoryBackward history match Nothing
            where
                match _ (Shout channel' nick' msg) Nothing
                    | channel == channel'
                      && nick == nick'
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
