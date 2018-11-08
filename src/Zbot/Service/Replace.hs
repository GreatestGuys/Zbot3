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
        handler (Shout channel _ msg) = lift $ do
            let maybeLineF = findLastMessage channel
            handleMessage (shout channel) maybeLineF msg
        handler _                        = return ()


        handleMessage reply maybeLineF msg =
            case split msg of
                ("s":from:to:"g":_) -> maybeLineF >>= handleReplace (replaceAll from to)
                ("s":from:to:_)     -> maybeLineF >>= handleReplace (replaceOne from to)
                _                   -> return ()
            where
                handleReplace f (Just (line, nick)) = let rline = (f line) in
                    if rline == line
                        then return ()
                        else reply $ nick `T.append` ": " `T.append` rline
                handleReplace _ Nothing = return ()

        findLastMessage channel = foldHistoryBackward history match Nothing
            where
                match _ (Shout channel' nick msg) Nothing
                    | channel == channel'
                      && not ("s/" `T.isPrefixOf` msg) = Just (msg, nick)
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
