{-# LANGUAGE RecordWildCards #-}
module Zbot.Core.Irc.Protocol (
    Prefix (..)
,   Message (..)
,   Parse (..)
,   Render (..)
) where

import Control.Applicative

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as T


data Prefix
    = ClientPrefix {
        nick :: T.Text
    ,   user :: T.Text
    ,   host :: T.Text
    }
    | ServerPrefix {
        server :: T.Text
    }
    deriving (Eq, Ord, Show, Read)

data Message
    = Message {
        prefix     :: Maybe Prefix
    ,   command    :: T.Text
    ,   parameters :: [T.Text]
    ,   trailing   :: Maybe T.Text
    } deriving (Eq, Ord, Show, Read)

--------------------------------------------------------------------------------
-- Rendering

class Render a where
    -- | Render a value into IRC wire format.
    render :: a -> T.Text

instance Render Message where
    render Message{..} = T.concat [
            prefixText
        ,   command
        ,   " "
        ,   T.intercalate " " parameters
        ,   trailingText
        ,   "\x0d\x0a"
        ]
        where
            trailingText | Just text <- trailing = " :" `T.append` text
                         | otherwise             = ""

            prefixText | Just justPrefix <- prefix = renderPrefix justPrefix
                       | otherwise                 = ""

            renderPrefix ServerPrefix{..} = T.concat [":", server, " "]
            renderPrefix ClientPrefix{..} =
                T.concat [":", nick, "!", user, "@", host, " "]

--------------------------------------------------------------------------------
-- Parsing

class Parse a where
    -- | Parse a value from IRC wire format.
    parse :: T.Text -> Maybe a

instance Parse Message where
    parse input = toMessage
                $ Atto.parse (parseMessage <* Atto.endOfInput) input
        where
            toMessage (Atto.Fail{})      = Nothing
            toMessage (Atto.Partial partial) = toMessage $ partial T.empty
            toMessage (Atto.Done _ result)   = Just result

parseMessage :: Atto.Parser Message
parseMessage = Message
    <$> parseMaybe parsePrefix
    <*> parseCommand
    <*> parseParams
    <*> parseMaybe parseTrailing
    <*  Atto.string "\x0d\x0a"

parsePrefix :: Atto.Parser Prefix
parsePrefix =
    Atto.char ':' >> Atto.choice [parseClientPrefix, parseServerPrefix]

parseClientPrefix :: Atto.Parser Prefix
parseClientPrefix = ClientPrefix
    <$> parseTextEndedBy1 '!'
    <*> parseTextEndedBy1 '@'
    <*> parseTextEndedBy1 ' '

parseServerPrefix :: Atto.Parser Prefix
parseServerPrefix = ServerPrefix
    <$> parseTextEndedBy1 ' '

parseCommand :: Atto.Parser T.Text
parseCommand = parseTextEndedBy1 ' '

parseParams :: Atto.Parser [T.Text]
parseParams = param `Atto.sepBy'` Atto.char ' '
    where param = T.cons <$> Atto.notChar ':' <*> Atto.takeWhile notTerminator
          notTerminator = (&&) <$> (/= ' ') <*> (/= '\x0d')

parseTrailing :: Atto.Parser T.Text
parseTrailing =
    Atto.takeWhile (== ' ') >> Atto.char ':' >> Atto.takeWhile (/= '\x0d')

parseTextEndedBy1 :: Char -> Atto.Parser T.Text
parseTextEndedBy1 char = Atto.takeWhile1 (/= char) <* Atto.char char

parseMaybe :: Atto.Parser a -> Atto.Parser (Maybe a)
parseMaybe parser = Atto.option Nothing (Just <$> parser)
