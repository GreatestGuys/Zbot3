module Zbot.Service.History.Log (
    appendLog
,   foldLogBackward
,   foldLogForward
)   where

import Zbot.Core.Irc
import Zbot.Service.History.Entry

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time
import Data.Time.Clock (UTCTime)
import Data.Word (Word16)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Marshal.Array (peekArray)

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import qualified System.IO.MMap as MMap


appendLog :: MonadIO io => FilePath -> Event -> io ()
appendLog path event = do
    entry <- mkEntry event
    liftIO $ LBS.appendFile path (Binary.encode entry)

foldLogForward :: MonadIO io
               => (UTCTime -> Event -> b -> b) -> b -> FilePath -> io b
foldLogForward f initial path = withLogFile path $ \(ptr, size) -> do
    let endOfFile = plusPtr ptr size
    loop endOfFile ptr initial
    where
        isEOF ptr eof = eof <= plusPtr ptr minEntrySize

        loop endOfFile ptr accum | isEOF ptr endOfFile = return accum
                                 | otherwise           = do
            entrySize <- fromIntegral <$> peekWord16 ptr
            Entry time event <- peekEntry entrySize ptr
            loop endOfFile (plusPtr ptr entrySize) (f time event accum)

foldLogBackward :: MonadIO io
                => (UTCTime -> Event -> b -> b) -> b -> FilePath -> io b
foldLogBackward f initial path = withLogFile path $ \(ptr, size) -> do
    let endOfFile = plusPtr ptr size
    loop ptr endOfFile initial
    where
        isSOF ptr sof = plusPtr ptr (negate minEntrySize) < sof

        loop startOfFile ptr accum | isSOF ptr startOfFile = return accum
                                   | otherwise             = do
            entrySize <- fromIntegral <$> peekWord16 (plusPtr ptr (-2))
            let entryPtr = plusPtr ptr (negate entrySize)
            Entry time event <- peekEntry entrySize entryPtr
            loop startOfFile entryPtr (f time event accum)

withLogFile :: MonadIO io => FilePath -> ((Ptr (), Int) -> IO a) -> io a
withLogFile path f = liftIO $ do
    touchFile path
    MMap.mmapWithFilePtr path MMap.ReadOnly Nothing f

minEntrySize :: Int
minEntrySize = 13  -- 2 * 2 bytes for size + 8 bytes for time + 1 byte for type.

peekEntry :: (Functor io, MonadIO io) => Int -> Ptr () -> io Entry
peekEntry size ptr = Binary.decode <$> liftIO (peekByteString size ptr)

peekByteString :: (Functor io, MonadIO io) => Int -> Ptr () -> io LBS.ByteString
peekByteString size ptr = LBS.pack <$> liftIO (peekArray size (castPtr ptr))

peekWord16 :: (Functor io, MonadIO io) => Ptr () -> io Word16
peekWord16 ptr = Binary.decode <$> liftIO (peekByteString 2 ptr)

touchFile :: FilePath -> IO ()
touchFile = (`LBS.appendFile` LBS.empty)
