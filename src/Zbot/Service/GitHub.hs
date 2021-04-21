{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zbot.Service.GitHub (
    GitHub
,   github
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
import Network.Wreq (defaults, postWith, responseBody, Options)
import Network.Wreq.Lens (header)

import qualified Control.Monad.Catch as Catch
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


type GitHubAccessToken = T.Text

data Project = Ircstats | Ggircd | Peefuck | Tnakchat | Zbot

toProject :: T.Text -> Maybe Project
toProject "irc-stats" = Just Ircstats
toProject "ggircd"    = Just Ggircd
toProject "peefuck"   = Just Peefuck
toProject "tnakchat"  = Just Tnakchat
toProject "zbot"      = Just Zbot
toProject "zbot3"     = Just Zbot
toProject _           = Nothing

getURL :: Project -> String
getURL Ircstats = "GreatestGuys/irc-stats"
getURL Ggircd   = "fimad/ggircd"
getURL Peefuck  = "fimad/pifuxelck"
getURL Tnakchat = "fimad/TnakChat"
getURL Zbot     = "GreatestGuys/Zbot3"

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
            helpAliases = ["!report", "!close"]
        ,   helpMessage = [
                    "usage: !report <project-name> Brief summary of issue"
                ,   "       !close <project-name> <issue-number>"
                ,   ""
                ,   "   The !report command can be used to file GitHub bugs"
                ,   "against specific projects. Please do not abuse this. It"
                ,   "will be taken away if you do."
                ,   ""
                ,   "  Supported projects-name's:"
                ,   "    - irc-stats"
                ,   "    - ggircd"
                ,   "    - peefuck"
                ,   "    - tnakchat"
                ,   "    - zbot"
                ]
        }
    }

handler :: (Catch.MonadCatch m, MonadIO m, Bot m)
        => MessageContext m
        -> T.Text
        -> MonadService GitHub m ()
handler reply msg
        | ("!report":project:words)   <- args =
            let issue = T.intercalate " " words
            in githubHandler (report project issue) reply
        | ["!close", project, number] <- args = githubHandler (close project number) reply
        | otherwise                           = return ()
        where
            report p i = maybe (emptyGitHubAction p) (reportIssue i) (toProject p)
            close  p i = maybe (emptyGitHubAction p) (closeIssue i) (toProject p)
            args       = T.words msg

type GitHubAction = forall m. (MonadIO m)
                  => MessageContext m -> Options -> m ()

emptyGitHubAction :: T.Text -> GitHubAction
emptyGitHubAction project ctx _ = reply ctx
                                $ "No project alias: " `T.append` project

reportIssue :: T.Text -> Project -> GitHubAction
reportIssue issue project ctx options = do
        result <- liftIO $ postWith options issueEndpoint payload
        reply ctx $ toIssueUrl result
        where
            payload = object ["title" .= issue]

            issueEndpoint =  "https://api.github.com/repos/"
                          ++ getURL project
                          ++ "/issues"

            -- The result is a JSON object, one of the fields is "url" which is the
            -- URL of the newly created issue.
            toIssueUrl result = fromMaybe errorMsg
                              $ result ^? responseBody . key "html_url" . _String

            errorMsg = "Encountered an error while filing the issue."

closeIssue :: T.Text -> Project -> GitHubAction
closeIssue number project ctx options = do
        result <- liftIO $ postWith options issueEndpoint payload
        reply ctx $ toIssueUrl result
        where
            payload = object ["state" .= ("closed" :: T.Text)]

            issueEndpoint =  "https://api.github.com/repos/"
                          ++ getURL project
                          ++ "/issues/"
                          ++ T.unpack number

            -- The result is a JSON object, one of the fields is "url" which is the
            -- URL of the newly created issue.
            toIssueUrl result = fromMaybe errorMsg
                              $ result ^? responseBody . key "html_url" . _String

            errorMsg = "Encountered an error while closing the issue."

githubHandler :: (Catch.MonadCatch m, MonadIO m, Bot m)
              => GitHubAction -> MessageContext m -> MonadService GitHub m ()
githubHandler h ctx =   gets unGitHub
                    >>= lift . maybe reportNotConfigured reportIssueSafe
    where
        tokenToOptions accessToken =
            let authHeader = T.encodeUtf8 $ "token " `T.append` accessToken
            in defaults & header "Authorization" .~ [authHeader]

        reportNotConfigured = reply ctx "GitHub service is not configured."

        reportIssueSafe = flip Catch.catch errorHandler . h ctx . tokenToOptions

        errorMsg = "Encountered an error while accessing GitHub api."

        errorHandler (_ :: Catch.SomeException) = reply ctx errorMsg
