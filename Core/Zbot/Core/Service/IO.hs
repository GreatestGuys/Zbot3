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
import Data.IORef
import Unsafe.Coerce


newtype IOCollective m a = MkIOCollective (StateT [Event -> IOCollective m ()] m a)
    deriving (Monad, Functor, Applicative)

instance MonadIO io => MonadIO (IOCollective io) where
    liftIO ioValue = MkIOCollective $ liftIO ioValue

instance MonadTrans IOCollective where
    lift value = MkIOCollective $ lift value

instance MonadIO io => Collective (IOCollective io) where

    data Handle a = MkHandle (IORef a)

    registerService service = MkIOCollective $ do
        ioref <- liftIO $ newIORef (initial service)
        modify (++ [handler ioref])
        return $ MkHandle ioref
        where
            handler ioref event = MkIOCollective $ do
                a <- liftIO $ readIORef ioref
                a' <- execStateT (unsafeCoerce a) (process service event)
                liftIO $ writeIORef ioref (unsafeCoerce a')

    run (MkHandle ioref) value = do
        a <- MkIOCollective $ liftIO $ readIORef ioref
        (result, a') <- runStateT value a
        MkIOCollective $ liftIO $ writeIORef ioref a'
        return result

    processEvent event = MkIOCollective get >>= mapM_ ($ event)

runIOCollective :: MonadIO io => IOCollective io a -> io a
runIOCollective (MkIOCollective value) = evalStateT value []
