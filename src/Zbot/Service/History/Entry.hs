{-# LANGUAGE RankNTypes #-}
module Zbot.Service.History.Entry (
    Entry (..)
,   mkEntry
)   where

import Zbot.Core.Irc

import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word8, Word16, Word64)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
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

instance Binary.Binary Event where
    get = do
        eventType <- getWord8
        case eventType of
            0 -> Shout <$> getText <*> getText <*> getText
            1 -> Whisper <$> getText <*> getText
            2 -> Join <$> getText <*> getText
            3 -> Part <$> getText <*> getText
            4 -> NickChange <$> getText <*> getText
            5 -> return Initialize
            _ -> mzero

    put (Shout channel nick message) = do
        putWord8 0
        putText channel
        putText nick
        putText message
    put (Whisper nick message) = do
        putWord8 1
        putText nick
        putText message
    put (Join channel nick) = do
        putWord8 2
        putText channel
        putText nick
    put (Part channel nick) = do
        putWord8 3
        putText channel
        putText nick
    put (NickChange from to) = do
        putWord8 4
        putText from
        putText to
    put Initialize = putWord8 5

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

getText :: Binary.Get T.Text
getText = do
    possiblyText <- (T.decodeUtf8' . LBS.toStrict) <$> Binary.get
    either (const mzero) return possiblyText

putText :: T.Text -> Binary.Put
putText = Binary.put . T.encodeUtf8
