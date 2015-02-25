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
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


data IOCollectiveMeta m = IOCollectiveMeta {
        metaDataDir  :: FilePath
    ,   metaHandlers :: [Event -> IOCollective m ()]
    }

newtype IOCollective m a = MkIOCollective
                           (StateT (IOCollectiveMeta m) m a)
    deriving (Monad, Functor, Applicative)

instance MonadIO io => MonadIO (IOCollective io) where
    liftIO ioValue = MkIOCollective $ liftIO ioValue

instance MonadTrans IOCollective where
    lift value = MkIOCollective $ lift value

instance MonadIO io => Collective (IOCollective io) where

    data Handle a = MkHandle (IORef (ServiceMeta a))

    registerService service = MkIOCollective $ do
        let serviceMeta = ServiceMeta (initial service) (name service)
        ioref <- liftIO $ newIORef serviceMeta
        modify $ addHandler (makeHandler ioref)
        return $ MkHandle ioref
        where
            addHandler handler meta = meta {
                    metaHandlers = metaHandlers meta ++ [handler]
                }
            makeHandler ioref event = do
                a <- liftIO $ readIORef ioref
                let (MkMonadService action) = (process service event)
                a' <- execStateT action a
                liftIO $ writeIORef ioref a'

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
        where
            normalize name = concat [
                    filter isAlphaNum $ T.unpack name
                ,   "-"
                ,   T.unpack $ T.decodeUtf8 $ SHA256.hash $ T.encodeUtf8 name
                ]

runIOCollective :: MonadIO io => FilePath -> IOCollective io a -> io a
runIOCollective dataDir (MkIOCollective value) = do
    liftIO $ createDirectoryIfMissing True dataDir
    evalStateT value $ IOCollectiveMeta dataDir []
