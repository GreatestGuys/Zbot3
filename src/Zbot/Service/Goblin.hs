{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Zbot.Service.Goblin (
    goblin
)   where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Message
import Zbot.Extras.Command
import Zbot.Extras.UnitService

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.List (foldl1')
import Data.UUID (UUID, toText)
import Data.UUID.V1 (nextUUID)
import Safe (readMay)

import System.Random

import qualified Data.Text as T


-- | A service that will generate a random SRD 3.5 goblin NPC.
goblin :: (MonadIO m, Bot m) => Service m ()
goblin =
    (unitService "Zbot.Service.Goblin" (onCommand "!goblin" handleCommand)) {
        helpSpec = Just HelpSpec {
                helpAliases = ["!goblin"]
            ,   helpMessage = ["usage: !goblin [-l level]"]
            }
    }


handleCommand :: (MonadIO m, Bot m)
              => MessageContext m -> T.Text -> MonadService () m ()
handleCommand ctx args = lift
                         $ makeGoblin (reply ctx) (parse args defaultOptions)

newtype Options = Options {optLevel :: Int}

defaultOptions = Options { optLevel = 1 }

data Goblin = Goblin {
              name :: T.Text
            , uuid :: T.Text
            , gender :: T.Text
            , level :: Int
            , str :: Int
            , dex :: Int
            , con :: Int
            , int :: Int
            , wis :: Int
            , cha :: Int
            , hp :: Int
            , skills :: [(Skill, Int)]
            , weapons :: [T.Text]
            } deriving Show

data Skill = Balance | Climb | Craft | EscapeArtist | HandleAnimal | Intimidate
           | Jump | Listen | Ride | Spot | Survival | Swim
           | UseRope
           deriving (Bounded, Enum)

instance Show Skill where
    show Balance = "balance"
    show Climb = "climb"
    show Craft = "craft"
    show EscapeArtist = "escape artist"
    show HandleAnimal = "handle animal"
    show Intimidate = "intimidate"
    show Jump = "jump"
    show Listen = "listen"
    show Ride = "ride"
    show Spot = "spot"
    show Survival = "survival"
    show Swim = "swim"
    show UseRope = "use rope"

parse :: T.Text -> Options -> Options
parse args opts
    | ("-l", Just l) <- (flag, intValue)  = parse rest $ opts {optLevel = l}
    | otherwise                           = opts
    where
        (flag, flagRest) = T.breakOn " " args
        (value, valueRest) = T.breakOn " "  $ T.stripStart flagRest
        rest = T.stripStart valueRest
        intValue = readMay $ T.unpack value

prettyGoblin :: Goblin -> [T.Text]
prettyGoblin Goblin{..} = [
        T.concat [ name, "(Figurative) (", uuid, "(Literal))"
                 ]
    ,   T.concat [ gender, " Goblin Warrior (", t level, ")"
                 ]
    ,   ""
    ,   T.concat [ "Str ", p 2 (t str)
                 , "    Dex ", p 2 (t dex)
                 , "    HP  ", p 2 (t hp)
                 ]
    ,   T.concat [ "Con ", p 2 (t con)
                 , "    Int ", p 2 (t int)
                 , "    AC  ", p 2 (t (10 + 1 + modifier dex))
                 ]
    ,   T.concat [ "Wis ", p 2 (t wis)
                 , "    Cha ", p 2 (t cha)
                 , "    Initiative ", p 2 (t (modifier dex))
                 ]
    ,   ""
    ,   T.concat [ "Fort   ", p 2 (t $ max 0 (level `div` 3))
                 , "   Size  Small"
                 ]
    ,   T.concat [ "Reflex ", p 2 (t $ max 0 (level `div` 3))
                 , "   Speed 30 ft."
                 ]
    ,   T.concat [ "Will   ", p 2 (t $ max 0 ((level + 4) `div` 2)) ]
    ,   ""
    ,   T.concat [ "Base Attack ", signedIntOr0 bab]
    ,   T.concat [ "Melee  ", signedIntOr0 (bab + modifier str) ]
    ,   T.concat [ "Ranged ", signedIntOr0 (bab + modifier dex) ]
    ,   ""
    ,   "Weapons:"
    ] ++ weapons ++ [
        ""
    ,   "Skills:"
    ] ++ prettySkills skills ++ [
        ""
    ,   "Languages:"
    ,   T.concat ["Goblin", if int >= 12 then ", Common" else ""]
    ]
    where
        bab = 1 + level
        p n t | n > T.length t = T.replicate (n - T.length t) " " `T.append` t
              | otherwise      = t
        t = T.pack . show
        signedInt n | n == 0    = ""
                    | n > 0     = T.concat ["+", T.pack $ show n]
                    | otherwise = T.pack $ show n
        signedIntOr0 n | n == 0    = "0"
                       | otherwise = signedInt n

prettySkills :: [(Skill, Int)] -> [T.Text]
prettySkills []     = []
prettySkills skills = T.intercalate ", " ( map toText
                                         $ take perLine nonZeroSkills)
                   : prettySkills (drop perLine nonZeroSkills)
    where perLine = 3
          nonZeroSkills = filter ((/= 0) . snd) skills
          toText (skill, value) = T.concat [ T.pack $ show skill
                                           , " "
                                           , T.pack $ show value
                                           ]

-- This method assumes that range is either null or contains a valid integer
-- value.
makeGoblin :: (MonadIO m, Bot m) => (T.Text -> m ()) -> Options -> m ()
makeGoblin reply opts = do
    baseGoblin <- startingGoblin
    goblin <- loop (optLevel opts - 1) levelUp baseGoblin
    mapM_ reply (prettyGoblin goblin)

loop :: MonadIO m => Int -> (a -> m a) -> a -> m a
loop n f a | n <= 0    = return a
           | otherwise = f a >>= loop (n - 1) f

initialSkills :: [(Skill, Int)]
initialSkills = zip [minBound .. maxBound] (repeat 0)

meleeWeapons :: [T.Text]
meleeWeapons = [
        "Longsword (1d6) (melee)"
    ,   "Morningstar (1d6) (melee)"
    ,   "Shortspear (1d4) (melee)"
    ]

rangedWeapons :: [T.Text]
rangedWeapons = [
        "Javelin (1d4) (ranged)"
    ,   "Light Crossbow (1d6) (ranged)"
    ,   "Sling (1d3) (ranged)"
    ]

-- Randomly generate a goblin with initial stats.
startingGoblin :: MonadIO m => m Goblin
startingGoblin = do
    name <- randList ["Ort", "Er", "Cronk", "Slozz", "Shig"]
    uuid <- getUUIDWithBackoff
    str <- 3 `d` 6
    dex <- 3 `d` 6
    con <- 3 `d` 6
    int <- 3 `d` 6
    wis <- 3 `d` 6
    cha <- 3 `d` 6
    skills <- buySkills ((4 + modifier int) * 4) initialSkills
    gender <- randList ["Male", "Female"]
    meleeWeapon <- randList meleeWeapons
    rangedWeapon <- randList rangedWeapons
    return Goblin {
        name = name
    ,   uuid = toText uuid
    ,   gender = gender
    ,   level = 1
    ,   str = str - 2
    ,   dex = dex + 2
    ,   con = con
    ,   int = int
    ,   wis = wis
    ,   cha = cha - 2
    ,   hp = 8 + modifier con
    ,   skills = skills
    ,   weapons = [meleeWeapon, rangedWeapon]
    }
    where
        getUUIDWithBackoff :: MonadIO m => m UUID
        getUUIDWithBackoff = liftIO $ do
          uuid <- nextUUID
          case uuid of
              Nothing     -> threadDelay 50000
                          >> getUUIDWithBackoff
              (Just uuid) -> return uuid


levelUp :: MonadIO m => Goblin -> m Goblin
levelUp goblin@Goblin{..} = do
    hpInc <- (+ modifier con) <$> roll 8
    return $ goblin {
               hp    = hp + hpInc
           ,   level = level + 1
           }

-- | Purchase ranks in a skill.
buySkills :: MonadIO m => Int -> [(Skill, Int)] -> m [(Skill, Int)]
buySkills ranks skills | ranks <= 0 = return skills
                       | otherwise  = do
    index <- (\x -> x - 1) <$> roll (length skills)
    let (skill, rank) = skills !! index
    buySkills (ranks - 1) $ concat [
                                     take index skills
                                   , [(skill, rank + 1)]
                                   , drop (index + 1) skills
                                   ]

-- | Calculate the modifier for a given ability score.
modifier :: Int -> Int
modifier n = (n - 10) `div` 2

randList :: MonadIO m => [a] -> m a
randList l = do
    index <- roll (length l)
    return $ l !! (index - 1)


d :: MonadIO m => Int -> Int -> m Int
d n sides = foldl1' (+) <$> replicateM n (roll sides)

roll :: MonadIO m => Int -> m Int
roll sides = do
    (result, _) <- liftIO $ fmap (randomR (1, sides)) newStdGen
    return result
