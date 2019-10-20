module Zbot.Extras.Regex (
    Reply
,   MessageContext(..)
,   MessageSource(..)
,   extractRegex
,   onRegex
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Message

import Data.Maybe (listToMaybe)
import Text.Regex.TDFA (makeRegexOpts, CompOption (..), ExecOption (..), Regex)
import Text.Regex.TDFA.Text ()

import qualified Data.Text as T
import qualified Text.Regex.TDFA as TDFA


type Pattern = T.Text

onRegex :: Bot m
        => Pattern
        -> (MessageContext m -> T.Text -> MonadService s m ())
        -> Event -> MonadService s m ()
onRegex pattern action =
        onMessage $ \reply -> mapM_ (action reply) . concat . match regex
    where regex = buildRegex pattern

extractRegex :: Pattern -> T.Text -> Maybe T.Text
extractRegex pattern = listToMaybe . concat . match regex
    where regex = buildRegex pattern

match :: Regex -> T.Text -> [[T.Text]]
match = TDFA.match

buildRegex :: Pattern -> Regex
buildRegex pattern = regex
    where
        -- | Generate a regex with specific configurations.
        regex       =   makeRegexOpts compOption execOption pattern
        compOption  =   CompOption {
                        caseSensitive   = True
                    ,   multiline       = True
                    ,   rightAssoc      = True
                    ,   newSyntax       = True
                    ,   lastStarGreedy  = True
                    }
        execOption  =   ExecOption {
                        captureGroups = False
                    }
