module Zbot.Service.Op (
    op
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.UnitService

import Control.Monad.Trans.Class (lift)

import qualified Data.Text as T

-- | A service that will grant ops to users on the current channel.
op :: Bot m => Service m ()
op = unitService "Zbot.Service.Op" handler

handler :: Bot m => Event -> MonadService () m ()
handler event = mapM_ ($ event) [ding, ascend, oprah]

ding :: Bot m => Event -> MonadService () m ()
ding (Shout channel nick "!ding") = lift $ addChannelMode channel [Op nick]
ding _                            = return()

ascend :: Bot m => Event -> MonadService () m ()
ascend (Shout channel nick "!ascend") = lift $ do
    addChannelMode channel [Op nick]
    shout channel $
        T.concat ["Welcome to the ranks of the Ascended, Brother ", nick, "."]
ascend _                              = return()

oprah :: Bot m => Event -> MonadService () m ()
oprah (Shout channel _ "!oprah") = lift $ do
    allNicks <- nicks channel
    mapM_ giveOp allNicks
    where
        giveOp nick = do
            addChannelMode channel [Op nick]
            shout channel $ "You get an OP!"
oprah _                          = return()
