module Zbot.Core.Service (
    Collective (registerService, run, sandboxedFilePath, helpSpecs)
,   Handle
,   Service (..)
,   MonadService
,   HelpSpec (..)

,   registerService_
) where

import Zbot.Core.Service.IO ()
import Zbot.Core.Service.Types

import Control.Monad


registerService_ :: (Functor m, Monad m, Collective m) => Service m a -> m ()
registerService_ service = void (registerService service)
