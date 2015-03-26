{-# LANGUAGE TupleSections #-}
module Zbot.Service.NGram.Model
where

import Control.Applicative
import Control.Arrow
import Data.Foldable (foldr', toList)
import Data.Int
import Data.List
import Data.Monoid
import Data.Random.RVar
import Data.Ratio

import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq


type N = Int

data Token a = Start | End | Token a
    deriving (Eq, Ord, Show, Read)

type Gram a = Seq.Seq (Token a)

type Distribution a = Distribution {
    distSize    :: Int64
,   distSamples :: HashMap.HashMap (Gram a) Int64
} deriving (Eq, Show, Read)

type Model a = Model (HashMap.HashMap a (Distribution a))
    deriving (Eq, Show, Read)

instance Ord a => Monoid (Distribution a) where
    mempty = Distribution 0 (HashMap.empty)
    mappend (Distribution sizeA distA) (Distribution sizeB distB)
        = Distribution {
            distSamples = HashMap.unionWith (+) distA distB
        ,   distSize    = sizeA + sizeB
        }

instance Ord a => Monoid (Model a) where
    mempty = Model (HashMap.empty)
    mappend (Model a) (Model b) = Model $ Map.unionWith (<>) a b

instance Show a => Show (Model a) where
    show (Model m) = concat $ toList $ HashMap.mapWithKey showModelEntry m
        where
            showModelEntry gram dist
                = concatMap (\d -> concat [showGram gram, "\t", d, "\n"])
                            (showDist dist)

            showGram = intercalate "\t" . map showToken . toList

            showDist (Distribution dist count)
                = map (\(t, p) -> showToken t ++ "\t" ++ show (toCount p))
                $ Map.assocs dist
                where toCount = numerator . (* fromIntegral count)

            showToken Start     = "Start"
            showToken End       = "End"
            showToken (Token a) = show a

-- |
makeModel :: Ord a => N -> [[a]] -> Model a
makeModel n = mconcat . map (ngramsToModel . ngrams n)

initial :: N -> [Token a]
initial n | n <= 1    = error "N must be greater than 1"
          | otherwise = replicate (n - 1) Start

ngrams :: N -> [a] -> [(Gram a, Token a)]
ngrams n rawTokens = map toGrams (takeValidTokens $ tails tokens)
    where tokens = initial n ++ map Token rawTokens ++ [End]
          toGrams = (,) <$> Seq.fromList . take (n - 1) <*> last . take n
          takeValidTokens = take (length rawTokens + 1)

ngramsToModel :: Ord a => [(Gram a, Token a)] -> Model a
ngramsToModel grams = Model model
    where
        gramToMap = map (second (`Map.singleton` 1)) grams
        counts = Map.fromListWith (Map.unionWith (+)) gramToMap
        toDist counts = Distribution ((% total) <$> counts) total
            where total = fromIntegral $ foldr' (+) 0 counts
        model = fmap toDist counts

-- |
score :: Model a -> [a] -> Rational
score = error "TODO"

-- |
generate :: Model a -> RVar [a]
generate = error "TODO"
