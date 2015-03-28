module Zbot.Core.Service (
    Collective (registerService, run, sandboxedFilePath)
,   Handle
,   Service (..)
,   MonadService

,   registerService_
) where

import Zbot.Core.Service.IO ()
import Zbot.Core.Service.Types

import Control.Monad


registerService_ :: (Monad m, Collective m) => Service m a -> m ()
registerService_ service = void (registerService service)
