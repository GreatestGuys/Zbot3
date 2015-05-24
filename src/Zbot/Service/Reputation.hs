module Zbot.Service.Reputation (
    Reputation
,   reputation
)   where

import Zbot.Core.Bot
import Zbot.Core.Irc
import Zbot.Core.Service
import Zbot.Extras.Message

import Control.Applicative ((<$>))
import Control.Monad.State
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Safe (readMay)

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Data.Text as T


newtype Reputation = MkReputation (Map.Map Nick Int)

-- | The lists service allows users to keep lists of things. It can be queried
-- with the !list command.
reputation :: Bot m => Service m Reputation
reputation = Service {
        initial     = MkReputation Map.empty
    ,   serialize   = serializeReputation
    ,   deserialize = deserializeReputation
    ,   name        = "Zbot.Service.Reputation"
    ,   process     = onMessage handler
    }

serializeReputation :: Reputation -> Maybe BS.ByteString
serializeReputation (MkReputation m) = Just $ BS.fromString $ show m

deserializeReputation :: BS.ByteString -> Maybe Reputation
deserializeReputation bs = MkReputation <$> readMay (BS.toString bs)

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
