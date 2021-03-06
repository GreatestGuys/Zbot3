{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Zbot.Core.Service.Types (
    Collective (..)
,   Service (..)
,   ServiceMeta (..)
,   MonadService (..)
,   HelpSpec (..)
,   serviceName
) where

import Zbot.Core.Irc

import Control.Monad.Fail (MonadFail)
import Control.Monad.State

import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString as BS
import qualified Data.Text as T


-- | A Service is a piece of state along with a processing function that is
-- capable of producing new state values based on IRC events. Each service has
-- a unique name, an initial state value, and the functions to serialize the
-- service's state to a byte string.
data Service m a = (Irc m, Collective m) => Service {
        initial     :: a
    ,   serialize   :: a -> Maybe BS.ByteString
    ,   deserialize :: BS.ByteString -> Maybe a
    ,   name        :: T.Text
    ,   process     :: Event -> MonadService a m ()
    ,   helpSpec    :: Maybe HelpSpec
    }

-- | A HelpSpec is a list of lines of text that are returned when a user
-- queries the documentation for a given service.
data HelpSpec = HelpSpec {
        helpAliases :: [T.Text]
    ,   helpMessage :: [T.Text]
    } deriving (Show)

data ServiceMeta a = ServiceMeta {
        metaValue :: a
    ,   metaName  :: T.Text
    }

newtype MonadService s m b = MkMonadService (StateT (ServiceMeta s) m b)
    deriving (Applicative,
              Catch.MonadCatch,
              Catch.MonadThrow,
              Functor,
              Monad,
              MonadFail,
              MonadIO,
              MonadTrans)

instance (Applicative m, Monad m) => MonadState s (MonadService s m) where
    get = MkMonadService $ metaValue <$> get

    put newValue = MkMonadService $ do
        oldState <- get
        put (oldState {metaValue = newValue})

-- | Obtaion the unique name of the current service.
serviceName :: (Applicative m, Monad m) => MonadService a m T.Text
serviceName = MkMonadService $ metaName <$> get

-- | A Collective is a group of managed services that can process IRC events.
class (Applicative m, Functor m, Monad m, MonadFail m) => Collective m where

    -- | An opaque handle that can be used to run operations in a state monad
    -- containing the state of the corresponding service. The first parameter is
    -- the type of the collective, and the second is the type of the service's
    -- state.
    data Handle m :: * -> *

    -- | Register a new service with the collective.
    registerService :: Monad m => Service m a -> m (Handle m a)

    -- | Run an operation in a state monad contain the state of the given
    -- service. Any changes to the state will be persisted.
    run :: Monad m => Handle m a -> MonadService a m b -> m b

    -- | Process an event by propagating it to each of the registered services.
    processEvent :: Monad m => Event -> m ()

    -- | Obtain the sandboxed file path for a given file name.
    sandboxedFilePath :: Monad m => T.Text -> MonadService a m FilePath

    -- | Return a list of tuples containing the service name, and the
    -- corresponding HelpSpec.
    helpSpecs :: m [(T.Text, Maybe HelpSpec)]
