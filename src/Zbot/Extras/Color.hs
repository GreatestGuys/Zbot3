module Zbot.Extras.Color (
    Color (..)
,   ColorText

,   bg
,   blink
,   bold
,   fg
,   strikeThrough
,   underline

,   colorize
)   where

import Data.String (IsString, fromString)

import qualified Data.Text as T


data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

data Style
    = Foreground Color
    | Background Color
    | Bold
    | Blink
    | Underline
    | StrikeThrough

data ColorText = ColorText [Style] T.Text

instance IsString ColorText where
    fromString str = ColorText [] (T.pack str)


codeFromColor :: Color -> T.Text
codeFromColor Black   = "0"
codeFromColor Red     = "1"
codeFromColor Green   = "2"
codeFromColor Yellow  = "3"
codeFromColor Blue    = "4"
codeFromColor Magenta = "5"
codeFromColor Cyan    = "6"
codeFromColor White   = "7"

codeFromStyle :: Style -> T.Text
codeFromStyle (Background color) = "4" `T.append` codeFromColor color
codeFromStyle (Foreground color) = "3" `T.append` codeFromColor color
codeFromStyle Blink              = "5"
codeFromStyle Bold               = "1"
codeFromStyle StrikeThrough      = "9"
codeFromStyle Underline          = "4"

-- | Colorize and concatenate a list of `ColorText`s.
colorize :: [ColorText] -> T.Text
colorize = T.concat . map go
  where go (ColorText styles text) = T.concat [
                "\x1b[", T.intercalate ";" (map codeFromStyle styles) ,"m"
            ,   text
            ,   "\x1b[0m"
            ]

addStyle :: Style -> ColorText -> ColorText
addStyle style (ColorText styles text) = ColorText (style : styles) text

-- | Set the ColorText's background color.
bg :: Color -> ColorText -> ColorText
bg = addStyle . Background

-- | Set the ColorText's foreground color.
fg :: Color -> ColorText -> ColorText
fg = addStyle . Foreground

-- | Make the ColorText blink.
blink :: ColorText -> ColorText
blink = addStyle Blink

-- | Make the ColorText bold.
bold :: ColorText -> ColorText
bold = addStyle Bold

-- | Make the ColorText crossed out.
strikeThrough :: ColorText -> ColorText
strikeThrough = addStyle StrikeThrough

-- | Make the ColorText underlined.
underline :: ColorText -> ColorText
underline = addStyle StrikeThrough
