{-# LANGUAGE OverloadedStrings #-}
module Zbot.Extras.Color (
    Color (..)
,   ColorText

,   bg
,   bold
,   fg
,   italic
,   strikeThrough
,   underline

,   colorize
,   colorText
)   where

import Data.String (IsString, fromString)

import qualified Data.Text as T


data Color
    = White
    | Black
    | Blue
    | Green
    | Red
    | Brown
    | Purple
    | Orange
    | Yellow
    | LightGreen
    | Cyan
    | LightCyan
    | LightBlue
    | Pink
    | Grey
    | LightGrey

data Style
    = Bold
    | Italic
    | Underline
    | StrikeThrough

data ColorText = ColorText (Maybe Color) (Maybe Color) [Style] T.Text

instance IsString ColorText where
    fromString str = ColorText Nothing Nothing [] (T.pack str)

codeFromColor :: Color -> T.Text
codeFromColor White      = "00"
codeFromColor Black      = "01"
codeFromColor Blue       = "02"
codeFromColor Green      = "03"
codeFromColor Red        = "04"
codeFromColor Brown      = "05"
codeFromColor Purple     = "06"
codeFromColor Orange     = "07"
codeFromColor Yellow     = "08"
codeFromColor LightGreen = "09"
codeFromColor Cyan       = "10"
codeFromColor LightCyan  = "11"
codeFromColor LightBlue  = "12"
codeFromColor Pink       = "13"
codeFromColor Grey       = "14"
codeFromColor LightGrey  = "15"

codeFromColorPair :: Maybe Color -> Maybe Color -> T.Text
codeFromColorPair Nothing  Nothing  = ""
codeFromColorPair (Just f) Nothing  = "\x03" `T.append` codeFromColor f
codeFromColorPair (Just f) (Just b) = T.concat [ "\x03", codeFromColor f
                                               , ",",    codeFromColor b ]
codeFromColorPair Nothing  (Just b) = T.concat [ "\x03", codeFromColor Black
                                               , ",",    codeFromColor b ]

codeFromStyle :: Style -> T.Text
codeFromStyle Bold               = "\x02"
codeFromStyle StrikeThrough      = "\x13"
codeFromStyle Underline          = "\x1f"
codeFromStyle Italic             = "\x09"

-- | Colorize and concatenate a list of `ColorText`s.
colorize :: [ColorText] -> T.Text
colorize = T.concat . map go
    where
        go (ColorText Nothing Nothing []     text) =  text
        go (ColorText f       b       styles text) =  T.concat
                                                   $  codeFromColorPair f b
                                                   :  map codeFromStyle styles
                                                   ++ [text, "\x0f"]

colorText :: T.Text -> ColorText
colorText = ColorText Nothing Nothing []

addStyle :: Style -> ColorText -> ColorText
addStyle style (ColorText f b styles text) = ColorText f b (style : styles) text

-- | Set the ColorText's background color.
bg :: Color -> ColorText -> ColorText
bg c (ColorText f _ s t) = ColorText f (Just c) s t

-- | Set the ColorText's foreground color.
fg :: Color -> ColorText -> ColorText
fg c (ColorText _ b s t) = ColorText (Just c) b s t

-- | Make the ColorText bold.
bold :: ColorText -> ColorText
bold = addStyle Bold

-- | Make the ColorText crossed out.
strikeThrough :: ColorText -> ColorText
strikeThrough = addStyle StrikeThrough

-- | Make the ColorText underlined.
underline :: ColorText -> ColorText
underline = addStyle StrikeThrough

-- | Make the ColorText underlined.
italic :: ColorText -> ColorText
italic = addStyle Italic
