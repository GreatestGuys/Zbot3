{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Zbot.Core.Service.IO (
    IOCollective
,   Service

,   runIOCollective
) where

import Zbot.Core.Irc
import Zbot.Core.Service.Types

import Control.Applicative
import Control.Monad.State
import Data.Char (isAlphaNum)
import Data.IORef
import Data.Time.Clock
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Control.Monad.Catch as Catch
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base16 as Hex


data IOCollectiveMeta m = IOCollectiveMeta {
        metaDataDir  :: FilePath
    ,   metaHandlers :: [Event -> IOCollective m ()]
    }

newtype IOCollective m a = MkIOCollective
                           (StateT (IOCollectiveMeta m) m a)
    deriving (Monad, Functor, Applicative, Catch.MonadCatch, Catch.MonadThrow)

instance MonadIO io => MonadIO (IOCollective io) where
    liftIO ioValue = MkIOCollective $ liftIO ioValue

instance MonadTrans IOCollective where
    lift value = MkIOCollective $ lift value

instance (Catch.MonadCatch io, MonadIO io) => Collective (IOCollective io) where

    data Handle a = MkHandle (IORef (ServiceMeta a))

    registerService service = MkIOCollective $ do
        dataDir <- gets metaDataDir
        let serviceDataDir = dataDir </> normalize (name service)
        let serviceMeta = ServiceMeta (initial service) (name service)
        liftIO $ createDirectoryIfMissing True serviceDataDir
        ioref <- liftIO $ newIORef serviceMeta
        modify $ addHandler (makeHandler ioref)
        return $ MkHandle ioref
        where
            addHandler handler meta = meta {
                    metaHandlers = metaHandlers meta ++ [handler]
                }
            makeHandler ioref event = suppressErrors event $ do
                a <- liftIO $ readIORef ioref
                let (MkMonadService action) = (process service event)
                a' <- execStateT action a
                liftIO $ writeIORef ioref a'

            suppressErrors event = flip Catch.catch (errorHandler event)
            errorHandler :: MonadIO m => Event -> Catch.SomeException -> m ()
            errorHandler event exception = liftIO $ do
                let marker c = putStrLn $ replicate 80 c
                let center msg = putStrLn $ T.unpack $ T.center 80 ' ' msg
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
        a <- MkIOCollective $ liftIO $ readIORef ioref
        (result, a') <- runStateT value a
        MkIOCollective $ liftIO $ writeIORef ioref a'
        return result

    processEvent event = MkIOCollective get >>= (mapM_ ($ event)) . metaHandlers

    sandboxedFilePath fileName = MkMonadService $ do
        dataDir <- lift $ MkIOCollective $ gets metaDataDir
        serviceName <- gets metaName
        return $ dataDir </> normalize serviceName </> normalize fileName

normalize :: T.Text -> FilePath
normalize name = concat [
        filter isAlphaNum $ T.unpack name
    ,   "-"
    ,   T.unpack $ T.decodeUtf8 $ Hex.encode $ SHA256.hash $ T.encodeUtf8 name
    ]

runIOCollective :: MonadIO io => FilePath -> IOCollective io a -> io a
runIOCollective dataDir (MkIOCollective value) = do
    liftIO $ createDirectoryIfMissing True dataDir
    evalStateT value $ IOCollectiveMeta dataDir []
