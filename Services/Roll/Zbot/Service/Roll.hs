module Zbot.Service.Roll (
    roll
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service

import Control.Monad.State
import Data.Char
import System.Random

import qualified Data.Text as T

newtype Roll = Roll ()

-- | A service that will roll a dice when a user messages "!roll".
roll :: (MonadIO m, Bot m) => Service m Roll
roll = Service {
    initial     = Roll ()
,   serialize   = const Nothing
,   deserialize = const Nothing
,   name        = "ZBot.Service.Roll"
,   process     = processEvent
}

processEvent :: (MonadIO m, Bot m) => Event -> StateT Roll m ()
processEvent (Shout channel _ msg) = lift $ processMessage (shout channel) msg
processEvent (Whisper nick msg)    = lift $ processMessage (whisper nick) msg
processEvent _                     = return ()

processMessage :: (MonadIO m, Bot m) => (T.Text -> m ()) -> T.Text -> m ()
processMessage reply message
    | "!roll " `T.isPrefixOf` message = rollDice reply sides
    | otherwise                       = return ()
    where sides = T.takeWhile isDigit $ T.drop 6 message

-- This method assumes that range is either null or contains a valid integer
-- value.
rollDice :: (MonadIO m, Bot m) => (T.Text -> m ()) -> T.Text -> m ()
rollDice reply range | T.null range || n <= 0 = return ()
                     | otherwise              = do
    (result, _) <- liftIO $ fmap (randomR (1, n)) newStdGen
    reply $ T.pack $ show result
    where n = read $ T.unpack range :: Integer
