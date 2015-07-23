module Zbot.Extras.Serialize (
    serializeShow
,   deserializeRead
) where

import Safe (readMay)

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS


serializeShow :: Show s => s -> Maybe BS.ByteString
serializeShow s = Just $ BS.fromString $ show s

deserializeRead :: Read r => BS.ByteString -> Maybe r
deserializeRead bs = readMay (BS.toString bs)
