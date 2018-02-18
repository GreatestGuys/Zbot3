{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Dude (
    dude
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service

import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Data.Time (diffUTCTime, UTCTime)

import Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M


-- Isomorphic to `Either (NonEmpty UTCTime) (NonEmpty UTCTime)`.
data Dudes = LeftDudes (NE.NonEmpty UTCTime)
           | RightDudes (NE.NonEmpty UTCTime)

data DudeType = LeftDude | RightDude

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
    | (Time t) <- event       =  updateTime t
    | (Shout channel _ input) <- event
    , isLeftDude input        =  handleDude channel LeftDude
    | (Shout channel _ input) <- event
    , isRightDude input       =  handleDude channel RightDude
    | otherwise               =  return ()
    where
        isLeftDude input  = any (== input) ["o/", "O/"]
        isRightDude input = any (== input) ["\\o", "\\O"]

        updateTime :: Bot m => UTCTime -> MonadService DudeState m ()
        updateTime t = do
            (DudeState _ m) <- get
            let dudesPartitioned = M.map (partitionDudes t) m
            let hangingDudes = M.mapMaybe fst dudesPartitioned
            let expiredDudes = M.mapMaybe snd dudesPartitioned
            M.traverseWithKey (\channel dudes -> lift $
                case dudes of
                    LeftDudes ds  -> replicateM_ (NE.length ds) (shout channel "\\o")
                    RightDudes ds -> replicateM_ (NE.length ds) (shout channel "o/"))
                              expiredDudes
            put $ DudeState (Just t) hangingDudes

        handleDude :: Bot m => Channel -> DudeType -> MonadService DudeState m ()
        handleDude channel dtype = do
            (DudeState t m) <- get
            case t of
                Nothing -> return ()
                Just t' -> put $ DudeState t (M.alter (updateDudes t' dtype) channel m)

-----------------------------------
-- Dude related helper functions --
-----------------------------------

-- | Given Dudes and the current time, partitions them into Dudes that
-- have been hanging for under 5 minutes and those that have been hanging
-- for over.
partitionDudes :: UTCTime -> Dudes -> (Maybe Dudes, Maybe Dudes)
partitionDudes t dudes   = case dudes of
    LeftDudes dudes  -> partitionDudes' t LeftDudes dudes
    RightDudes dudes -> partitionDudes' t RightDudes dudes
    where
        partitionDudes' t duder dudes =
            let (hanging, expired) = NE.partition (\d -> (t `diffUTCTime` d) < 300) dudes
            in  (mkDudes duder hanging, mkDudes duder expired)

        mkDudes duder dudes = fmap duder $ NE.nonEmpty dudes

-- | Either adds a new dude to the hanging list or pops the oldest dude
-- depending on whether or not the dudetype passed in is the same or
-- the opposite of the dudes currently hanging.
updateDudes :: UTCTime -> DudeType -> Maybe Dudes -> Maybe Dudes
updateDudes t LeftDude Nothing                    = Just $ LeftDudes $ t :| []
updateDudes t LeftDude (Just (LeftDudes dudes))   = Just $ LeftDudes $ NE.cons t dudes
updateDudes _ LeftDude (Just (RightDudes dudes))  = fmap RightDudes
                                                  $ NE.nonEmpty
                                                  $ NE.init dudes
updateDudes t RightDude Nothing                   = Just $ RightDudes $ t :| []
updateDudes t RightDude (Just (RightDudes dudes)) = Just $ RightDudes $ NE.cons t dudes
updateDudes _ RightDude (Just (LeftDudes dudes))  = fmap LeftDudes
                                                  $ NE.nonEmpty
                                                  $ NE.init dudes
