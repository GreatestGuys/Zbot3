{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Zbot.Core.Service.Types (
     Collective (..)
 ,   Service (..)
) where

import Zbot.Core.Irc

import Control.Monad.State

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
,   process     :: Event -> StateT a m ()
}

-- | A Collective is a group of managed services that can process IRC events.
class Collective m where

    -- | An opaque handle that can be used to run operations in a state monad
    -- containing the state of the corresponding service.
    data Handle :: * -> *

    -- | Register a new service with the collective.
    registerService :: Monad m => Service m a -> m (Handle a)

    -- | Run an operation in a state monad contain the state of the given
    -- service. Any changes to the state will be persisted.
    run :: Monad m => Handle a -> StateT a m b -> m b

    -- | Process an event by propagating it to each of the registered services.
    processEvent :: Monad m => Event -> m ()
