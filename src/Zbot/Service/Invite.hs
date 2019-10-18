{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Invite (
    acceptInvite
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.UnitService

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)

-- | A service that will accept invitations to join a channel.
acceptInvite :: Bot m => Service m ()
acceptInvite = unitService "Zbot.Service.Invite" handler

handler :: Bot m => Event -> MonadService () m ()
handler (Invite invitee channel)  = lift $ do
    nick <- myNick
    when (nick == invitee) $ joinChannel channel
handler (Shout channel _ "!gtfo") = lift $ partChannel channel
handler _                         = return ()
