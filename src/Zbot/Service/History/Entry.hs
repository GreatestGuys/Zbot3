{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Zbot.Service.History.Entry (
    Entry (..)
,   mkEntry
)   where

import Zbot.Core.Irc

import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Word (Word8, Word16, Word64)

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


data Entry = Entry UTCTime Event

mkEntry :: MonadIO io => Event -> io Entry
mkEntry event = do
    time <- liftIO getCurrentTime
    return $ Entry time event

instance Binary.Binary Entry where
    get = do
        _         <- getWord16
        timestamp <- Binary.get
        event     <- Binary.get
        _         <- getWord16
        return $ Entry timestamp event

    put (Entry timestamp event) = do
        let timeBytes = encodedSize timestamp
        let eventBytes = encodedSize event
        let totalSize = timeBytes + eventBytes + 4
        putWord16 totalSize
        Binary.put timestamp
        Binary.put event
        putWord16 totalSize
        where
            encodedSize :: forall b. Binary.Binary b => b -> Word16
            encodedSize = fromIntegral . LBS.length . Binary.encode

instance Binary.Binary UTCTime where
    get = (posixSecondsToUTCTime . fromIntegral) <$> getWord64
    put = putWord64 . round . utcTimeToPOSIXSeconds

-- | Unique labels for our event types
data EventType = Zero | One | Two | Three | Four | Five | Six deriving (Data)

instance Binary.Binary Event where
    get = do
        eventType <- getEventType
        case eventType of
            Just Zero  -> Shout <$> getText <*> getText <*> getText
            Just One   -> Whisper <$> getText <*> getText
            Just Two   -> Join <$> getText <*> getText
            Just Three -> Part <$> getText <*> getText
            Just Four  -> NickChange <$> getText <*> getText
            Just Five  -> return Initialize
            Just Six   -> Invite <$> getText <*> getText
            Nothing    -> mzero

    put (Shout channel nick message) = do
        putEventType Zero
        putText channel
        putText nick
        putText message
    put (Whisper nick message) = do
        putEventType One
        putText nick
        putText message
    put (Join channel nick) = do
        putEventType Two
        putText channel
        putText nick
    put (Part channel nick) = do
        putEventType Three
        putText channel
        putText nick
    put (NickChange from to) = do
        putEventType Four
        putText from
        putText to
    put Initialize = putEventType Five
    put (Invite nick channel) = do
        putEventType Six
        putText nick
        putText channel
    put (Time _) = return ()

getWord8 :: Binary.Get Word8
getWord8 = Binary.get

putWord8 :: Word8 -> Binary.Put
putWord8 = Binary.put

getWord16 :: Binary.Get Word16
getWord16 = Binary.get

putWord16 :: Word16 -> Binary.Put
putWord16 = Binary.put

getWord64 :: Binary.Get Word64
getWord64 = Binary.get

putWord64 :: Word64 -> Binary.Put
putWord64 = Binary.put

getEventType :: Binary.Get (Maybe EventType)
getEventType = do
    eventType <- fmap toInt getWord8
    case M.lookup eventType eventTypeMapping of
        Nothing  -> return Nothing
        (Just c) -> return $ Just $ fromConstr c
    where
        eventTypeMapping = M.fromList
                         . map (\x -> (pred $ constrIndex x, x))
                         . dataTypeConstrs
                         $ dataTypeOf eventType

        eventType :: EventType
        eventType = undefined


        toInt :: Word8 -> Int
        toInt = fromIntegral

getText :: Binary.Get T.Text
getText = do
    possiblyText <- (T.decodeUtf8' . LBS.toStrict) <$> Binary.get
    either (const mzero) return possiblyText

putEventType :: EventType -> Binary.Put
putEventType e = putWord8
               . fromIntegral
               . pred
               . constrIndex
               $ toConstr e

putText :: T.Text -> Binary.Put
putText = Binary.put . T.encodeUtf8
