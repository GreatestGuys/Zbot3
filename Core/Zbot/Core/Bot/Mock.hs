{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Zbot.Core.Bot.Mock (
    MockBot
,   runMockBot
) where

import Zbot.Core.Bot.Types
import Zbot.Core.Irc
import Zbot.Core.Irc.Engine
import Zbot.Core.Irc.Protocol
import Zbot.Core.Service.IO
import Zbot.Core.Service.Types hiding (Handle)

import Control.Monad.State

import qualified Data.Text as T
import qualified Data.Text.IO as T

type MockBot = IOCollective (StateT EngineState IO)

instance MonadState EngineState MockBot where
    get = lift get
    put = lift . put

instance Irc MockBot where
    sendMessage priority message = do
        liftIO $ T.putStr $ T.concat [
                "[->IRC] "
            ,   "(", T.pack $ show priority, ") "
            ,   render message
            ]

instance Bot MockBot where

runMockBot :: FilePath -> MockBot () -> [Event] -> IO ()
runMockBot dataDir botInit events =
    flip evalStateT undefined $
        runIOCollective dataDir $ do
            startEngine "mockBot" "mockBot"
            botInit
            mapM_ processEvent events
