{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Zbot.Core.Service.IO (
    IOCollective
,   Service

,   runIOCollective
) where

import Zbot.Core.Irc
import Zbot.Core.Service.Types
import Zbot.Metrics

import Control.Monad.Fail (MonadFail)
import Control.Monad.State
import Data.Char (isAlphaNum)
import Data.IORef
import Data.Ratio ((%))
import Data.Time.Clock
import System.Clock (Clock(..), diffTimeSpec, getTime, toNanoSecs)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Control.Monad.Catch as Catch
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Prometheus as P


data IOCollectiveMeta m = IOCollectiveMeta {
        metaDataDir   :: FilePath
    ,   metaHandlers  :: [Event -> IOCollective m ()]
    ,   metaHelpSpecs :: [(T.Text, Maybe HelpSpec)]
    }

newtype IOCollective m a = MkIOCollective
                           (StateT (IOCollectiveMeta m) m a)
    deriving (
        Monad,
        Functor,
        Applicative,
        Catch.MonadCatch,
        Catch.MonadThrow,
        MonadFail)

instance MonadIO io => P.MonadMonitor (IOCollective io) where
    doIO = liftIO

instance MonadIO io => MonadIO (IOCollective io) where
    liftIO ioValue = MkIOCollective $ liftIO ioValue

instance MonadTrans IOCollective where
    lift value = MkIOCollective $ lift value

instance (Applicative io,
          Functor io,
          Catch.MonadCatch io,
          MonadFail io,
          MonadIO io) => Collective (IOCollective io) where

    data Handle (IOCollective io) a =
            MkHandle (IORef (ServiceMeta a, Service (IOCollective io) a))

    registerService service = do
        serviceMeta <- readFromSaveFile
                            (ServiceMeta (initial service) (name service))
                            service
        MkIOCollective $ do
            dataDir <- gets metaDataDir
            let serviceDataDir = dataDir </> (normalize (name service) ++ ".d")
            liftIO $ createDirectoryIfMissing True serviceDataDir
            ioref <- liftIO $ newIORef (serviceMeta, service)
            modify $ addHandlerAndHelp
                        (makeHandler ioref)
                        (name service, helpSpec service)
            return $ MkHandle ioref
        where
            addHandlerAndHelp handler help meta = meta {
                    metaHandlers = metaHandlers meta ++ [handler]
                ,   metaHelpSpecs = help : metaHelpSpecs meta
                }

            timeHandler handler = do
                start  <- liftIO $ getTime Monotonic
                result <- handler
                end    <- liftIO $ getTime Monotonic
                --- Convert nano seconds to seconds, % as this is a rational.
                let duration = toNanoSecs (end `diffTimeSpec` start) % 1000000000
                liftIO $ P.withLabel metricHandlerSeconds (name service) $
                    \c -> void $ P.observe c (fromRational duration)
                return result

            makeHandler ioref event = timeHandler $ suppressErrors event $ do
                (m, s) <- liftIO $ readIORef ioref
                let (MkMonadService action) = process service event
                m' <- execStateT action m
                liftIO $ writeIORef ioref (m', s)
                writeToSaveFile m' s

            suppressErrors event = flip Catch.catch (errorHandler event)
            errorHandler :: MonadIO m => Event -> Catch.SomeException -> m ()
            errorHandler event exception = liftIO $ do
                let marker c = putStrLn $ replicate 80 c
                let center msg = putStrLn $ T.unpack $ T.center 80 ' ' msg
                liftIO $ P.withLabel metricErrors (name service) P.incCounter
                now <- getCurrentTime
                marker '='
                center "BEGIN ERROR REPORT"
                marker '-'
                putStrLn $ "Service:   " ++ T.unpack (name service)
                putStrLn $ "Event:     " ++ show event
                putStrLn $ "Timestamp: " ++ show now
                putStrLn $ "Exception: " ++ show exception
                marker '-'
                center "END ERROR REPORT"
                marker '='

    run (MkHandle ioref) (MkMonadService value) = do
        (m, s) <- MkIOCollective $ liftIO $ readIORef ioref
        (result, m') <- runStateT value m
        MkIOCollective $ liftIO $ writeIORef ioref (m', s)
        writeToSaveFile m' s
        return result

    processEvent event = MkIOCollective get >>= mapM_ ($ event) . metaHandlers

    sandboxedFilePath fileName = MkMonadService $ do
        dataDir <- lift $ MkIOCollective $ gets metaDataDir
        serviceName <- gets metaName
        return $ dataDir </> (normalize serviceName ++ ".d")
                         </> normalize fileName

    helpSpecs = MkIOCollective $ gets metaHelpSpecs

writeToSaveFile :: MonadIO io
                => ServiceMeta a
                -> Service (IOCollective io) a
                -> IOCollective io ()
writeToSaveFile meta@(ServiceMeta value _) service = do
    saveFile <- serviceSaveFile meta
    let serializedValue = serialize service value
    liftIO $ writeTo saveFile serializedValue
    where
        writeTo _    Nothing        = return ()
        writeTo path (Just valueBS) = BS.writeFile path valueBS

readFromSaveFile :: MonadIO io
                 => ServiceMeta a
                 -> Service (IOCollective io) a
                 -> IOCollective io (ServiceMeta a)
readFromSaveFile meta service = do
    saveFile <- serviceSaveFile meta
    liftIO $ do
        touchFile saveFile
        serializedValue <- liftIO $ BS.readFile saveFile
        let maybeValue = deserialize service serializedValue
        return $ maybe meta (flip ServiceMeta (metaName meta)) maybeValue

serviceSaveFile :: MonadIO io => ServiceMeta a -> IOCollective io FilePath
serviceSaveFile (ServiceMeta _ serviceName) = do
    dataDir <- MkIOCollective $ gets metaDataDir
    return (dataDir </> normalize serviceName)

normalize :: T.Text -> FilePath
normalize name = concat [
        filter isAlphaNum $ T.unpack name
    ,   "-"
    ,   take 7 $ T.unpack $ sha256 name
    ]
    where sha256 = T.decodeUtf8 . Hex.encode . SHA256.hash . T.encodeUtf8

runIOCollective :: MonadIO io => FilePath -> IOCollective io a -> io a
runIOCollective dataDir (MkIOCollective value) = do
    liftIO $ createDirectoryIfMissing True dataDir
    evalStateT value $ IOCollectiveMeta dataDir [] []

touchFile :: FilePath -> IO ()
touchFile = (`BS.appendFile` BS.empty)
