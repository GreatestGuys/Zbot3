module Zbot.Service.Grep (
    grep
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Command
import Zbot.Extras.UnitService
import Zbot.Service.History

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

import qualified Data.Text as T


-- | A service that will grant ops to users on the current channel.
grep :: (MonadIO m, Bot m) => Handle History -> Service m ()
grep historyHandle =
    unitService "Zbot.Service.Grep" (onCommand "!grep" grepCommand)
    where
        grepCommand reply args = lift $ do
            (result, _) <- foldHistoryBackward
                            historyHandle
                            (match (parse args defaultOptions))
                            initialGrepResult
            maybe (return ()) reply result

        matchFirst query time (Shout channel nick msg) (Nothing, i)
            | i == 0       = (Nothing, next i)
            | msg =~ query = (describe time channel nick msg, next i)
            | otherwise    = (Nothing, next i)
        matchFirst _     _    _                        (r, i) = (r, next i)

        next i = i `seq` i + 1

data GrepOptions = GrepOptions {
        context :: Int
    ,   matches :: Int
    ,   nick    :: Maybe Nick
    ,   channel :: Maybe Channel
    ,   query   :: String
    }

data GrepResult = GrepResult {
        isFirst    :: Bool
    ,   numMatched :: Int
    ,   results    :: [[T.Text]]
    }

defaultOptions :: GrepOptions
defaultOptions = GrepOptions {
        context = ,
    ,   matches = 1
    ,   nick = Nothing
    ,   channel = Nothing
    ,   query = Nothing
    }

parse :: T.Text -> GrepOptions -> GrepOptions
parse args options
    | ("-m", Just m)  <- (flag, intValue)  = parse rest $ options {matches = m}
    | ("-c", Just c)  <- (flag, intValue)  = parse rest $ options {context = c}
    | ("-C", c)       <- (flag, justValue) = parse rest $ options {channel = c}
    | ("-n", n)       <- (flag, justValue) = parse rest $ options {nick = n}
    | otherwise                            = options {query = args}
    where
        (flag, flagRest) = T.breakOn " " args
        (value, valueRest) = T.breakOn " "  $ T.stripStart flagRest
        rest = T.stripStart valueRest
        intValue = readMaybe $ T.unpack value
        justValue = Just value

match :: GrepOptions -> UTCTime -> Event -> GrepResult -> GrepResult

describe :: UTCTime -> Channel -> Nick -> T.Text -> T.Text
describe time channel nick message = T.concat [
        T.pack $ show time, " " , channel, " ", nick, "> ", message
    ]
