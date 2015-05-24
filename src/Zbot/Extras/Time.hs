module Zbot.Extras.Time (
    prettyDiffTime
)   where

import Control.Arrow (first)
import Data.List (foldl')
import Data.Time.Clock

import qualified Data.Text as T


-- | Creates a pretty printed version of a NomimalDiffTime.
prettyDiffTime :: NominalDiffTime -> T.Text
prettyDiffTime td = T.pack $ unwords
            $   map (uncurry (++) . first show)
            $   if null diffs
                    then [(0, "s")]
                    else diffs
    where
        merge (tot,acc) (sec,typ) =
            let (sec', tot') = divMod tot sec
            in (tot', (sec', typ):acc)

        metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]

        diffs   = filter ((/=0) . fst)
                $ reverse
                $ snd
                $ foldl' merge (round td, []) metrics
