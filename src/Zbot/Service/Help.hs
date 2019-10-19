{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Help (
    help
)   where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.UnitService
import Zbot.Extras.Command

import Control.Monad.Trans.Class (lift)
import Data.Maybe (mapMaybe, listToMaybe)
import GHC.Exts (sortWith)

import qualified Data.Text as T


help :: Bot m => Service m ()
help = (unitService "Zbot.Service.Help" (onCommands ["!help", "!halp"] handler)) {
        helpSpec = Just HelpSpec {
                helpAliases      = ["!help", "!halp"]
            ,   helpMessage      = [
                    "usage: !help [command]"
                ,   ""
                ,   "   The !help command is used to display usage and other"
                ,   "general information about a component. When no argument is"
                ,   "given, this message will be displayed."
                ]
            }
        }

handler :: Bot m => Reply m -> T.Text -> MonadService () m ()
handler reply arg
    | T.null arg = lift $ do
        Just helpMsg <- lookupHelpSpec "!help"
        replyHelpMsg helpMsg
        services <- servicesWithHelp
        mapM_ (reply Direct) ["", "Services with help pages:", ""]
        mapM_ replyServiceAndSpec services
    | otherwise  = lift $ lookupHelpSpec arg >>= maybe replyNone replyHelpMsg
    where
        replyHelpMsg = mapM_ (reply Direct) . helpMessage
        replyNone = reply Direct "There is no such help page."
        replyServiceAndSpec (service, spec) = reply Direct $ T.concat [
                "    ", service, ": ", T.intercalate ", " (helpAliases spec)
            ]

lookupHelpSpec :: Bot m => T.Text -> m (Maybe HelpSpec)
lookupHelpSpec query =   listToMaybe
                     .   filter (elem query . helpAliases)
                     .   mapMaybe snd
                     <$> helpSpecs

servicesWithHelp :: Bot m => m [(T.Text, HelpSpec)]
servicesWithHelp = sortWith fst . mapMaybe filterMaybe <$> helpSpecs
    where
        filterMaybe (_, Nothing)      = Nothing
        filterMaybe (name, Just spec) = Just (name, spec)
