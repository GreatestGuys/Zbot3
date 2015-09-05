module Zbot.Service.Reputation (
    Reputation
,   reputation
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Message
import Zbot.Extras.Serialize

import Control.Monad.State
import Data.List (sortBy)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T


newtype Reputation = MkReputation (Map.Map Nick Int) deriving (Read, Show)

reputation :: Bot m => Service m Reputation
reputation = Service {
        initial     = MkReputation Map.empty
    ,   serialize   = serializeReputation
    ,   deserialize = deserializeReputation
    ,   name        = "Zbot.Service.Reputation"
    ,   process     = onMessage handler
    ,   helpSpec    = Just HelpSpec {
            helpAliases      = ["!rep", "+1", "-1"]
        ,   helpMessage      = [
            "usage: +1 [nick]"
        ,   "       -1 [nick]"
        ,   "       !rep [nick]"
        ,   ""
        ,   "   The reputation service keeps track of the 'reputation' of"
        ,   "users in the channel. Reputation here being an arbitrary integer"
        ,   "value. Users can grant each other reputation via the `+1` command."
        ,   "If no user is supplied, the last nick that spoke something other"
        ,   "than a `+1` command will gain 1 reputation point. If a nick is"
        ,   "supplied, then that nick will be granted the point."
        ,   ""
        ,   "   The !rep command is used to query reputation scores. If no nick"
        ,   "Is given, then the reputation score of all known nicks will be"
        ,   "displayed. Otherwise, if a nick is given, then only the reputation"
        ,   "score for that user is displayed."
        ]
        }
    }

serializeReputation :: Reputation -> Maybe BS.ByteString
serializeReputation (MkReputation m) = serializeShow m

deserializeReputation :: BS.ByteString -> Maybe Reputation
deserializeReputation = fmap MkReputation . deserializeRead

handler :: Bot m => Reply m -> T.Text -> MonadService Reputation m ()
handler reply msg
    | ["+1", nick]   <- args = plus nick
    | ["-1", nick]   <- args = minus nick
    | ["!rep", nick] <- args = replyNickRep nick
    | ["!rep"]       <- args = replyAllRep
    | otherwise              = return ()
    where
        args = T.words msg

        plus  = modifyRep (+ 1)
        minus = modifyRep (subtract 1)

        modifyRep f nick = wrapModify (Map.alter f' nick)
            where f' = return . f . fromMaybe 0

        replyNickRep nick =   wrapGets (fromMaybe 0 . Map.lookup nick)
                          >>= replyRep nick

        replyAllRep = do
            pairs <- wrapGets Map.assocs
            let sorted = sortBy (\(_,a) (_,b) -> compare b a) pairs
            mapM_ (uncurry replyRep) sorted

        replyRep nick rep = lift
                          $ reply
                          $ T.concat [nick, " has ", showText rep, " rep"]

wrapModify :: Bot m
           => (Map.Map Nick Int -> Map.Map Nick Int)
           -> MonadService Reputation m ()
wrapModify f = modify (\(MkReputation m) -> MkReputation $ f m)

wrapGets :: Bot m
         => (Map.Map Nick Int -> a)
         -> MonadService Reputation m a
wrapGets f = gets $ \(MkReputation m) -> f m

showText :: Show a => a -> T.Text
showText = T.pack . show