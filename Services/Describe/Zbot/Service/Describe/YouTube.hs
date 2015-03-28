module Zbot.Service.Describe.YouTube (
    describeYouTube
) where

import Zbot.Extras.Scrape
import Zbot.Service.Describe

import Control.Applicative
import Control.Monad
import Data.Aeson
import Text.HTML.Scalpel

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


describeYouTube :: Describer
describeYouTube url
    | isYouTubeLink url = Just $ scrapeURLAsMobile (toEmbedLink url) info
    | otherwise         = Nothing

isYouTubeLink :: T.Text -> Bool
isYouTubeLink = (||) <$> ("youtube.com/watch" `T.isInfixOf`)
                     <*> ("youtu.be/" `T.isInfixOf`)

toEmbedLink :: T.Text -> T.Text
toEmbedLink url
    | "youtu.be" `T.isInfixOf` url = T.replace "tu.be" "tube.com/embed" url
    | "watch?v=" `T.isInfixOf` url = T.replace "watch?v=" "embed/" url
    | otherwise                    =
        let (_, videoId) = T.breakOn "v=" url
        in "http://www.youtube.com/embed/" `T.append` T.drop 2 videoId

info :: Scraper T.Text T.Text
info = do
    script <- T.concat <$> (texts ("body" :: T.Text) // ("script" :: T.Text))
    case decode $ extractJson script of
        Nothing                                       -> empty
        Just (YouTubeInfo title seconds views rating) -> return $ T.concat [
                "["
            ,   "rating: ", toStarRating rating, ", "
            ,   "length: ", toTime seconds, ", "
            ,   "views: ", T.pack $ show views
            ,   "] "
            ,   title
            ]

extractJson :: T.Text -> LBS.ByteString
extractJson script = LBS.fromStrict $ T.encodeUtf8 json
    where
        jsonPrefix           = "yt.setConfig('PLAYER_CONFIG',"
        (_, jsonWithGarbage) = T.breakOn jsonPrefix script
        jsonWithTrailing     = T.drop (T.length jsonPrefix) jsonWithGarbage
        (json, _)            = T.breakOn ");" jsonWithTrailing


toStarRating :: Double -> T.Text
toStarRating rating | rating < 0.5 = "☆☆☆☆☆"
                    | rating < 1.5 = "★☆☆☆☆"
                    | rating < 2.5 = "★★☆☆☆"
                    | rating < 3.5 = "★★★☆☆"
                    | rating < 4.5 = "★★★★☆"
                    | otherwise    = "★★★★★"

toTime :: Int -> T.Text
toTime seconds = T.intercalate ":" [hourText, minuteText, secondText]
    where
        secondText = formatText $ seconds `mod` 60
        minuteText = formatText $ seconds `div` 60 `mod` 60
        hourText = formatText $ seconds `div` 60 `div` 60
        formatText = T.justifyRight 2 '0' . T.pack . show

data YouTubeInfo = YouTubeInfo T.Text Int Integer Double

instance FromJSON YouTubeInfo where
    parseJSON (Object v) = v .: "args" >>= \args -> YouTubeInfo
        <$> args .: "title"
        <*> args .: "length_seconds"
        <*> args .: "view_count"
        <*> args .: "avg_rating"
    parseJSON _          = mzero
