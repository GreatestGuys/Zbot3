{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Zbot.Service.NGram.Model
where

import Control.Applicative
import Control.Arrow
import Data.Foldable (foldr', toList)
import Data.Int
import Data.List
import Data.Monoid
import Data.Proxy
import Data.Random.RVar
import Data.Ratio
import GHC.TypeLits (KnownNat, Nat, natVal, type (<=))

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq


data Token a = Start | End | Token a
    deriving (Eq, Ord, Show, Read)

type Gram (n :: Nat) a = Seq.Seq (Token a)

data Distribution a = Distribution {
        distSize    :: Int64
    ,   distSamples :: Map.Map (Token a) Int64
    } deriving (Eq, Show, Read)

data Model (n :: Nat) a = Model (Map.Map (Gram n a) (Distribution a))
    deriving (Eq, Read)

instance Ord a => Monoid (Distribution a) where
    mempty = Distribution 0 (Map.empty)
    mappend (Distribution sizeA distA) (Distribution sizeB distB)
        = Distribution {
            distSamples = Map.unionWith (+) distA distB
        ,   distSize    = sizeA + sizeB
        }

instance Ord a => Monoid (Model n a) where
    mempty = Model (Map.empty)
    mappend (Model a) (Model b) = Model $ Map.unionWith (<>) a b

instance Show a => Show (Model n a) where
    show (Model m) = concat $ toList $ Map.mapWithKey showModelEntry m
        where
            showModelEntry gram dist
                = concatMap (\d -> concat [showGram gram, "\t", d, "\n"])
                            (showDist dist)

            showGram = intercalate "\t" . map showToken . toList

            showDist (Distribution count dist)
                = map (\(t, c) -> showToken t ++ "\t" ++ show c)
                $ Map.assocs dist

            showToken Start     = "Start"
            showToken End       = "End"
            showToken (Token a) = show a

-- |
makeModel :: forall n a. (KnownNat n, 2 <= n, Ord a) => [[a]] -> Model n a
makeModel = mconcat . map (ngramsToModel . ngrams n)
    where n = (Proxy :: Proxy n)

initial :: (KnownNat n, 2 <= n) => Proxy n -> [Token a]
initial n = replicate (nLit - 1) Start
    where nLit = fromIntegral $ natVal n

ngrams :: (KnownNat n, 2 <= n) => Proxy n -> [a] -> [(Gram n a, Token a)]
ngrams n rawTokens = map toGrams (takeValidTokens $ tails tokens)
    where nLit = fromIntegral $ natVal n
          tokens = initial n ++ map Token rawTokens ++ [End]
          toGrams = (,) <$> Seq.fromList . take (nLit - 1)
                        <*> last . take nLit
          takeValidTokens = take (length rawTokens + 1)

ngramsToModel :: (KnownNat n, 2 <= n, Ord a)
              => [(Gram n a, Token a)] -> Model n a
ngramsToModel grams = Model model
    where
        gramToMap = map (second (`Map.singleton` 1)) grams
        counts = Map.fromListWith (Map.unionWith (+)) gramToMap
        toDist c = Distribution total c where total = Map.foldr (+) 0 c
        model = fmap toDist counts

-- |
score :: Model n a -> [a] -> Rational
score = error "TODO"

-- |
generate :: Model n a -> RVar [a]
generate = error "TODO"
