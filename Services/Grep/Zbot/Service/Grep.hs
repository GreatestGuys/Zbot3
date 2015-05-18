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
grep :: (MonadIO m, Bot m) => Handle m History -> Service m ()
grep historyHandle =
    unitService "Zbot.Service.Grep" (onCommand "!grep" grepCommand)
    where
        grepCommand reply query = lift $ do
            (result, _) <- foldHistoryBackward
                            historyHandle
                            (matchFirst query)
                            (Nothing, 0)
            maybe (return ()) reply result

        matchFirst query time (Shout channel nick msg) (Nothing, i)
            | i == 0       = (Nothing, next i)
            | msg =~ query = (describe time channel nick msg, next i)
            | otherwise    = (Nothing, next i)
        matchFirst _     _    _                        (r, i) = (r, next i)

        next i = i `seq` i + 1

        describe time channel nick message = Just $ T.concat [
                T.pack $ show time, " " , channel, " ", nick, "> ", message
            ]
