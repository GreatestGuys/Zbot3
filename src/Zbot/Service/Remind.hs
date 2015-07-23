module Zbot.Service.Remind (
    Remind
,   remind
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Serialize

import Control.Monad.State
import Data.List (partition)

import qualified Data.Text as T


newtype Remind = MkRemind {
        unMkRemind :: [(Nick, Channel, Nick, T.Text)]
    }
    deriving (Read, Show)

remind :: Bot m => Service m Remind
remind = Service {
        initial     = MkRemind []
    ,   serialize   = serializeShow
    ,   deserialize = deserializeRead
    ,   name        = "Zbot.Service.Remind"
    ,   process     = handler
    ,   helpSpec    = Just HelpSpec {
            helpAliases      = ["!remind"]
        ,   helpMessage      = [
                "usage: !remind nick message"
            ,   ""
            ,   "   The remind service allows users to set reminders for"
            ,   "members of the current channel. The next time this user joins"
            ,   "or speaks in the channel the reminders will be replayed in the"
            ,   "channel."
            ]
        }
    }

handler :: Bot m => Event -> MonadService Remind m ()
handler (Shout channel nick msg) =  handleRemind channel nick
                                 >> handleCmd channel nick msg
handler (Join channel nick)      =  handleRemind channel nick
handler _                        =  return ()

handleCmd :: Bot m => Channel -> Nick -> T.Text -> MonadService Remind m ()
handleCmd channel nick msg
    | "!remind":other:xs <- T.words msg = modify (addReminder other xs)
    | otherwise                         = return ()
    where addReminder other xs = MkRemind
                               . ((other, channel, nick, T.unwords xs) :)
                               . unMkRemind

handleRemind :: Bot m => Channel -> Nick -> MonadService Remind m ()
handleRemind channel nick = do
    (reminders, remaining) <- gets (partition reminderMatches . unMkRemind)
    put (MkRemind remaining)
    lift $ mapM_ sendReminder $ reverse reminders
    where reminderMatches (n, ch, _, _) = nick == n && ch == channel

sendReminder :: Bot m => (Nick, Channel, Nick, T.Text) -> m ()
sendReminder (nick, channel, owner, message) = shout channel $ T.concat [
        nick, ", ", message, " ~ ", owner
    ]
