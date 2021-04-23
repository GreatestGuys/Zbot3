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

import Control.Lens ((&), (.~), (^?), (^.), filtered, only, has)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Aeson ((.=), object)
import Data.Aeson.Lens (key, values, _String)
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import Network.Wreq (defaults, get, getWith, postWith, responseBody, Options)
import Network.Wreq.Lens (header)

import qualified Control.Monad.Catch as Catch
import qualified Data.Text as T
import qualified Data.Text.Conversions as T
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
            helpAliases = ["!report", "!close", "!emoji"]
        ,   helpMessage = [
                    "usage: !report <project-name> Brief summary of issue"
                ,   "       !close <project-name> <issue-number>"
                ,   "       !emoji <emoji> <image-url>"
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
                ,   ""
                ,   "   The !emoji command can be used to automatically submit"
                ,   "pull requests to TnakChat to add emojis. The <emoji> value"
                ,   "must only contain alphanumberic characters and/or '_'. The"
                ,   "URL must be either a PNG or a GIF."
                ,   ""
                ,   "Adding multiple emojis before merging previous pull"
                ,   "requests may result in merge conflicts that require manual"
                ,   "editing."
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
        | ["!close", project, number] <- args =
            githubHandler (close project number) reply
        | ["!emoji", emoji, url]      <- args =
            githubHandler (addTnakChatEmoji emoji url) reply
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

addTnakChatEmoji :: T.Text -> T.Text -> GitHubAction
addTnakChatEmoji emoji url ctx options
  | not $ T.all (\c -> c == '_' || isAlphaNum c) emoji = do
    reply ctx "Emoji contains invalid characters; must be alphanumberic or '_'"
  | otherwise = do
    let apiBase = "https://api.github.com/repos/fimad/tnakchat/"
    let gitBase = apiBase ++ "git/"
    let emoji = T.toLower emoji
    void $ runMaybeT $ do
      -- Obtain a reference to HEAD on master.
      (headSha, headUrl) <- MaybeT $ liftIO $ do
        result <- getWith options $ gitBase ++ "ref/heads/master"
        let url = result ^? responseBody . key "object" . key "url" . _String
        let sha = result ^? responseBody . key "object" . key "sha" . _String
        return $ (,) <$> sha <*> url

      -- Get a reference to the tree that corresponds to the head commit.
      (treeSha, treeUrl) <- MaybeT $ liftIO $ do
        result <- getWith options $ T.unpack headUrl
        return $ (,)
               <$> result ^? responseBody . key "tree" . key "sha" . _String
               <*>  result ^? responseBody . key "tree" . key "url" . _String

      -- Find the emoji.js file within the current tree.
      let emojiJsPath = "web/components/message-group/decorators/emoji.js"
      emojiJsUrl <- MaybeT $ liftIO $ do
        result <- getWith options $ T.unpack treeUrl ++ "?recursive=1"
        let hasTargetPath = has (key "path" . _String . only emojiJsPath)
        return $ result ^? responseBody
                        . key "tree"
                        . values . filtered hasTargetPath
                        . key "url" . _String

      -- Grab the content of the emoji.js file so that it can be updated.
      emojiJsBase64 <- MaybeT $ liftIO $ do
        result <- getWith options $ T.unpack emojiJsUrl
        return $ result ^? responseBody . key "content" . _String
      let decodeBase64 base64 = T.decodeUtf8 . T.unBase64
                              <$> T.convertText (T.replace "\n" "" base64)
      emojiJs <- MaybeT $ return $ decodeBase64 emojiJsBase64

      -- Edit the emojiJs file to include the new emoji.
      let extension = if ".gif" `T.isInfixOf` url then ".gif" else ".png"
      let emojiImageName = emoji `T.append` extension
      let edit = T.unlines [
            "const customEmoji = {",
            T.concat ["  ':", emoji, ":': {"],
            "    type: 'image',",
            T.concat ["    value: 'emoji/", emojiImageName, "',"],
            "  },"]
      let newEmojiJs = T.replace "const customEmoji = {\n" edit emojiJs

      -- Download the emoji image to memory.
      newEmojiImage <- liftIO $ do
          response <- get $ T.unpack url
          return $ response ^. responseBody

      -- Upload a file new blob containing the edited emoji JS.
      newEmojiJsSha <- MaybeT $ liftIO $ do
        let payload = object ["content" .= newEmojiJs]
        result <- postWith options (gitBase ++ "blobs") payload
        return $ result ^? responseBody . key "sha" . _String

      -- Upload a file new blob containing the edited emoji JS.
      newEmojiImageSha <- MaybeT $ liftIO $ do
        let payload = object [
              "content" .= (T.convertText $ T.Base64 newEmojiImage :: T.Text),
              "encoding" .= ("base64" :: T.Text)]
        result <- postWith options (gitBase ++ "blobs") payload
        return $ result ^? responseBody . key "sha" . _String

      -- Create a new tree containing the edited emoji JS file and the emoji
      -- image.
      let emojiImagePath = "static/emoji/" `T.append` emojiImageName
      newTreeSha <- MaybeT $ liftIO $ do
        let payload = object [
              "base_tree" .= treeSha,
              "tree" .= [
                  object [
                      "path" .= emojiJsPath,
                      "sha" .= newEmojiJsSha,
                      "mode" .= ("100644" :: T.Text),
                      "type" .= ("blob" :: T.Text)
                  ],
                  object [
                      "path" .= emojiImagePath,
                      "sha" .= newEmojiImageSha,
                      "mode" .= ("100644" :: T.Text),
                      "type" .= ("blob" :: T.Text)
                  ]]]
        result <- postWith options (gitBase ++ "trees") payload
        return $ result ^? responseBody . key "sha" . _String

      -- Create a commit that contains the tree with the edited JS file and new
      -- emoji image.
      commitSha <- MaybeT $ liftIO $ do
        let payload = object [
              "message" .= T.concat ["Add :", emoji, ": as an emoji"],
              "tree" .= newTreeSha,
              "parents" .= [headSha]]
        result <- postWith options (gitBase ++ "commits") payload
        return $ result ^? responseBody . key "sha" . _String

      -- Create a new branch that points to the new commit. A branch is required
      -- for creating a pull request.
      let branchName = "zbot-add-new-emoji-" `T.append` emoji
      let refName = "refs/heads/" `T.append` branchName
      MaybeT $ liftIO $ do
        let payload = object [
              "ref" .= refName,
              "sha" .= commitSha]
        result <- postWith options (gitBase ++ "refs") payload
        return $ result ^? responseBody . key "ref" . _String

      -- Create a pull request that contains the suggested emoji.
      pullUrl <- MaybeT $ liftIO $ do
        let payload = object [
              "title" .= T.concat ["Add :", emoji, ": as an emoji"],
              "head" .= branchName,
              "base" .= ("master" :: T.Text),
              "maintainer_can_modify" .= True]
        result <- postWith options (apiBase ++ "pulls") payload
        return $ result ^? responseBody . key "html_url" . _String

      lift $ reply ctx pullUrl

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
