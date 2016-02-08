{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zbot.Service.GitHub (
    GitHub
,   github
) where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Command
import Zbot.Extras.Serialize

import Control.Lens ((&), (.~), (^?))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Control.Monad.Trans.Class (lift)
import Data.Aeson ((.=), object)
import Data.Aeson.Lens (key, _String)
import Data.Maybe (fromMaybe)
import Network.Wreq (defaults, postWith, responseBody)
import Network.Wreq.Lens (param)

import qualified Control.Monad.Catch as Catch
import qualified Data.Text as T


type GitHubAccessToken = T.Text

newtype GitHub = MkGitHub {
        unGitHub :: Maybe GitHubAccessToken
    }
    deriving (Read, Show)

github :: (Catch.MonadCatch m, MonadIO m, Bot m) => Service m GitHub
github = Service {
        initial     = MkGitHub Nothing
    ,   serialize   = serializeShow
    ,   deserialize = deserializeRead
    ,   name        = "Zbot.Service.GitHub"
    ,   process     = onCommand "!report" handleCommand
    ,   helpSpec    = Just HelpSpec {
            helpAliases = ["!report"]
        ,   helpMessage = [
                    "usage: !report Brief summary of issue"
                ,   ""
                ,   "   The !report command can be used to file GitHub bugs"
                ,   "against the ZBot3 project. Please do not abuse this. It"
                ,   "will be taken away if you do."
                ]
        }
    }

handleCommand :: (Catch.MonadCatch m, MonadIO m, Bot m)
              => Reply m -> T.Text -> MonadService GitHub m ()
handleCommand reply issue =   gets unGitHub
                          >>= lift . maybe reportNotConfigured reportIssueSafe
    where
        issueEndpoint = "https://api.github.com/repos/GreatestGuys/Zbot3/issues"

        reportNotConfigured = reply "GitHub service is not configured."

        reportIssueSafe = flip Catch.catch errorHandler . reportIssue

        reportIssue accessToken = do
            let options = defaults & param "access_token" .~ [accessToken]
            let payload = object ["title" .= issue]
            result <- liftIO $ postWith options issueEndpoint payload
            reply $ toIssueUrl result

        -- The result is a JSON object, one of the fields is "url" which is the
        -- URL of the newly created issue.
        toIssueUrl result = fromMaybe errorMsg
                          $ result ^? responseBody . key "html_url" . _String

        errorMsg = "Encountered an error while filing the issue."

        errorHandler (_ :: Catch.SomeException) = reply errorMsg
