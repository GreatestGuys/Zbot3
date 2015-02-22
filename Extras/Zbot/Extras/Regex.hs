module Zbot.Extras.Regex (
    onRegex
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Extras.Message

import Control.Monad.State
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

import qualified Data.Text as T

type Pattern = T.Text

onRegex :: Bot m
        => Pattern
        -> (Reply m -> T.Text -> StateT s m ())
        -> Event -> StateT s m ()
onRegex pattern action =
        onMessage $ \reply -> mapM_ (action reply) . concat . match regex
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
