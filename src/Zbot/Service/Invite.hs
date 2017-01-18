{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Invite (
    acceptInvite
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.UnitService

import Control.Monad.Trans.Class (lift)

-- | A service that will accept invitations to join a channel.
acceptInvite :: Bot m => Service m ()
acceptInvite = unitService "Zbot.Service.Invite" handler

handler :: Bot m => Event -> MonadService () m ()
handler (Invite invitee channel) = lift $ do
    nick <- myNick
    if (nick == invitee)
        then joinChannel channel
        else return ()
handler _                        = return ()
