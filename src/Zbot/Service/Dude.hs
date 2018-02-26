{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Zbot.Service.Dude (
    dude
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service

import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap, first)
import Data.Bitraversable (bitraverse)
import Data.Time (diffUTCTime, UTCTime)

import Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T


type LatestTime = Maybe UTCTime

type HangingDudes = M.Map Channel Dudes

data DudeState = DudeState LatestTime HangingDudes

-- | The dude service never leaves a friend hanging.
dude :: Bot m => Service m DudeState
dude = Service {
        initial     = DudeState Nothing M.empty
    ,   serialize   = const Nothing
    ,   deserialize = const Nothing
    ,   name        = "Zbot.Service.Dude"
    ,   process     = dudeHandler
    ,   helpSpec    = Nothing
    }

dudeHandler :: Bot m => Event -> MonadService DudeState m ()
dudeHandler event
    | (Time t)                <- event
                               = updateTime t >> expireDudes
    | (Shout channel _ input) <- event
                               = handleShout channel input
    | otherwise                = return ()
    where
        handleShout channel input
            | isLeftDude input  = handleDude channel LeftDude
            | isRightDude input = handleDude channel RightDude
            | otherwise         = updateLines channel
            where
                isLeftDude input  = any (== input) ["o/", "O/"]
                isRightDude input = any (== input) ["\\o", "\\O"]

                updateLines :: Bot m => Channel -> MonadService DudeState m ()
                updateLines channel = do
                    (DudeState t m) <- get
                    let m' = M.adjust ageDudes channel m
                    put $ DudeState t m'

                handleDude :: Bot m => Channel -> DudeType -> MonadService DudeState m ()
                handleDude channel dtype = do
                    (DudeState t m) <- get
                    case t of
                        Nothing -> return ()
                        Just t' -> put $ DudeState t (M.alter (updateDudes t' dtype) channel m)

        updateTime :: Bot m => UTCTime -> MonadService DudeState m ()
        updateTime t = do
            (DudeState _ m) <- get
            put $ DudeState (Just t) m

        expireDudes :: Bot m => MonadService DudeState m ()
        expireDudes = do
            (DudeState (Just t) m) <- get
            let dudesPartitioned = M.map (partitionDudes $ isExpired t) m
            let hangingDudes = M.mapMaybe snd dudesPartitioned
            let expiredDudes = M.mapMaybe fst dudesPartitioned
            M.traverseWithKey (\channel (dudes, dtype) -> lift $
                replicateM_ (NE.length dudes) (shout channel . showDude $ flipDude dtype))
                              expiredDudes
            put $ DudeState (Just t) hangingDudes

----------------------------------------------------------------
--------------------------- Dude API ---------------------------
----------------------------------------------------------------

data Dude = Dude {
            dudeBirthday :: UTCTime -- Timestamp for original o/
          , dudeAge :: Int          -- # of non-dude lines spoken
          }

type Dudes = (NE.NonEmpty Dude, DudeType)

data DudeType =
-- | There once was a dude who hung left,
--   whose pants grew quite tight from his heft.
--     A week since last blown
--     and at home all alone,
--   his hand by itself proved quite deft.
                LeftDude
-- | There once was a dude who hung right,
--   who found himself needy one night.
--     Three swipes til a match,
--     his tool down the hatch,
--   he wanted a kiss, not a bite.
              | RightDude
              deriving Eq

-- | A dude without a birthday
--      is no dude at all.
--                \o/
--                | |
--                |-|
mkDude :: UTCTime -> Dude
mkDude dudeBirthday = Dude {dudeAge = 0, ..}

-- | All alone a-dude awaits
--    soulmates separated
-- o/ ......................
-- time trickles.. tediously
-- Forever,,,,,,,,,,,,,,,,,,
--  ...... ..... \o ... ...
-- . .... ....... o/ .... ..
-- .. .. .......? o ?.......
-- ,,,  ,,,,,,,,,,,,,,alone?
flipDude :: DudeType -> DudeType
flipDude LeftDude  = RightDude
flipDude RightDude = LeftDude

-- | I'll show you mine
showDude :: DudeType -> T.Text
-- if you show
showDude LeftDude  = "o/"
-- me yours. :>
showDude RightDude = "\\o"

-- | Another line goes by,
-- in the blink of an eye.
--
-- How old these dudes grow
-- the most ancient of bros.
ageDudes :: Dudes -> Dudes
ageDudes dudes = first (fmap ageDude) dudes
    where
        ageDude Dude{..} = Dude{dudeAge=dudeAge+1, ..}

-- | a dude is too old /
-- if they've hung for five minutes /
-- or seen five plus lines
isExpired :: UTCTime -> Dude -> Bool
isExpired t Dude{..} =  dudeAge >= 5
                     || (t `diffUTCTime` dudeBirthday) >= 300

-- | A function from dudes to truths,
-- gives a function from dudes to dudes and dudes.
-- Or not!
partitionDudes :: (Dude -> Bool)
               -> Dudes -> (Maybe Dudes, Maybe Dudes)
partitionDudes p (dudes,dt) = mapTuple (bitraverseFst NE.nonEmpty)
                            . mapTuple (\x -> (x, dt))
                            $ NE.partition p dudes
    where
        mapTuple = join bimap
        bitraverseFst f = bitraverse f pure

-- | Bikens a UTCTime and DudeType follunly. If jishith
-- Maybe Dudes then ranak a pazmow so givvishly.
--  (e.g. updateDudes 0 RightDude Nothing)
updateDudes :: UTCTime -> DudeType -> Maybe Dudes -> Maybe Dudes
updateDudes t new Nothing             = Just (mkDude t :| [], new)
updateDudes t new (Just (dudes, old))
    | new == old                      = Just (NE.cons (mkDude t) dudes, old)
    | otherwise                       = fmap (\x -> (x, old))
                                      . NE.nonEmpty
                                      $ NE.init dudes
