{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zbot.Service.GitHub (
    GitHub
,   github
,   reportIssue
) where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Message
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
    ,   process     = onMessage handler
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

handler :: (Catch.MonadCatch m, MonadIO m, Bot m)
        => Reply m
        -> T.Text
        -> MonadService GitHub m ()
handler reply msg
        | ("!report":issue)    <- args = githubHandler reportIssue reply $ T.concat issue
        | ("!close":number:[]) <- args = githubHandler closeIssue reply number
        | otherwise                    = return ()
        where
            args = T.words msg

type GitHubAction = forall m. (MonadIO m) => Reply m -> T.Text -> GitHubAccessToken -> m ()

reportIssue :: GitHubAction
reportIssue reply issue accessToken = do
        result <- liftIO $ postWith options issueEndpoint payload
        reply $ toIssueUrl result
        where
            options = defaults & param "access_token" .~ [accessToken]

            payload = object ["title" .= issue]

            issueEndpoint = "https://api.github.com/repos/GreatestGuys/Zbot3/issues"

            -- The result is a JSON object, one of the fields is "url" which is the
            -- URL of the newly created issue.
            toIssueUrl result = fromMaybe errorMsg
                              $ result ^? responseBody . key "html_url" . _String

            errorMsg = "Encountered an error while filing the issue."

closeIssue :: GitHubAction
closeIssue reply number accessToken = do
        result <- liftIO $ postWith options issueEndpoint payload
        reply $ toIssueUrl result
        where
            options = defaults & param "access_token" .~ [accessToken]

            payload = object ["state" .= ("closed" :: T.Text)]

            issueEndpoint =  "https://api.github.com/repos/GreatestGuys/Zbot3/issues/"
                          ++ T.unpack number

            -- The result is a JSON object, one of the fields is "url" which is the
            -- URL of the newly created issue.
            toIssueUrl result = fromMaybe errorMsg
                              $ result ^? responseBody . key "html_url" . _String

            errorMsg = "Encountered an error while closing the issue."


githubHandler :: (Catch.MonadCatch m, MonadIO m, Bot m)
              => GitHubAction -> Reply m -> T.Text -> MonadService GitHub m ()
githubHandler h reply issue =   gets unGitHub
                            >>= lift . maybe reportNotConfigured reportIssueSafe
    where
        reportNotConfigured = reply "GitHub service is not configured."

        reportIssueSafe = flip Catch.catch errorHandler . (h reply issue)

        errorMsg = "Encountered an error while accessing GitHub api."

        errorHandler (_ :: Catch.SomeException) = reply errorMsg
