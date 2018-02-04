{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Zbot.Core.Bot.Mock (
    MockBot
,   Output (..)
,   evalMockBot
,   runMockBot
) where

import Zbot.Core.Bot.Types
import Zbot.Core.Irc
import Zbot.Core.Irc.Engine
import Zbot.Core.Irc.Protocol
import Zbot.Core.Service.IO
import Zbot.Core.Service.Types hiding (Handle)

import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Text as T
import qualified Data.Text.IO as T

type MockBot = IOCollective (WriterT [Output] (StateT EngineState IO))

data Output = Output Priority Message
    deriving (Eq, Show)

instance MonadState EngineState MockBot where
    get = lift $ lift get
    put = lift . lift . put

instance Irc MockBot where
    sendMessage priority message = do
        lift $ tell $ [Output priority message]

instance Bot MockBot where

runMockBot :: FilePath -> MockBot () -> [Event] -> IO ()
runMockBot dataDir botInit events = evalMockBot dataDir botInit events
                                  >>= mapM_ (T.putStrLn . T.pack . show)

evalMockBot :: FilePath -> MockBot () -> [Event] -> IO [Output]
evalMockBot dataDir botInit events =
    flip evalStateT undefined $
        execWriterT $
            runIOCollective dataDir $ do
                startEngine "mockBot" "mockBot" "mockPass"
                botInit
                mapM_ processEvent events
