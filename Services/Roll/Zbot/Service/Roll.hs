module Zbot.Service.Roll (
    roll
)   where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Command
import Zbot.Extras.UnitService

import Control.Monad.State
import Data.Char
import System.Random

import qualified Data.Text as T

type Roll = ()

-- | A service that will roll a dice when a user messages "!roll".
roll :: (MonadIO m, Bot m) => Service m Roll
roll = unitService "Zbot.Service.Roll" (command "!roll" handleCommand)

handleCommand :: (MonadIO m, Bot m) => Reply m -> [T.Text] -> StateT Roll m ()
handleCommand reply [arg] = lift $ rollDice reply (T.takeWhile isDigit arg)
handleCommand _     _     = return ()

-- This method assumes that range is either null or contains a valid integer
-- value.
rollDice :: (MonadIO m, Bot m) => (T.Text -> m ()) -> T.Text -> m ()
rollDice reply range | T.null range || n <= 0 = return ()
                     | otherwise              = do
    (result, _) <- liftIO $ fmap (randomR (1, n)) newStdGen
    reply $ T.pack $ show result
    where n = read $ T.unpack range :: Integer
