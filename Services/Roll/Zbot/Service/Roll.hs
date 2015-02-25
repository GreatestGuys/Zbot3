module Zbot.Service.Roll (
    roll
)   where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Message
import Zbot.Extras.Command
import Zbot.Extras.UnitService

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)

import Data.Char
import System.Random

import qualified Data.Text as T

-- | A service that will roll a dice when a user messages "!roll".
roll :: (MonadIO m, Bot m) => Service m ()
roll = unitService "Zbot.Service.Roll" (onCommand "!roll" handleCommand)

handleCommand :: (MonadIO m, Bot m)
              => Reply m -> T.Text -> MonadService () m ()
handleCommand reply arg = lift $ rollDice reply (T.takeWhile isDigit arg)

-- This method assumes that range is either null or contains a valid integer
-- value.
rollDice :: (MonadIO m, Bot m) => (T.Text -> m ()) -> T.Text -> m ()
rollDice reply range | T.null range || n <= 0 = return ()
                     | otherwise              = do
    (result, _) <- liftIO $ fmap (randomR (1, n)) newStdGen
    reply $ T.pack $ show result
    where n = read $ T.unpack range :: Integer
