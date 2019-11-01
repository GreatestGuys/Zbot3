{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Zbot.Service.NGram.Model (
    Model
,   makeModel
,   generate
) where

import Control.Arrow (second)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.List (intercalate, tails)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import Data.Random (RVar, uniform)
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

newtype Model (n :: Nat) a = Model (Map.Map (Gram n a) (Distribution a))
    deriving (Eq, Read, Semigroup)

instance Ord a => Monoid (Distribution a) where
    mempty = Distribution 0 Map.empty
    mappend (Distribution sizeA distA) (Distribution sizeB distB)
        = Distribution {
            distSamples = Map.unionWith (+) distA distB
        ,   distSize    = sizeA + sizeB
        }

instance Ord a => Monoid (Distribution a) where
    mempty = Distribution 0 Map.empty

instance Ord a => Semigroup (Model n a) where
    (<>) (Model a) (Model b) = Model $ Map.unionWith (<>) a b

instance Ord a => Monoid (Model n a) where
    mempty = Model Map.empty

instance Show a => Show (Model n a) where
    show (Model m) = concat $ toList $ Map.mapWithKey showModelEntry m
        where
            showModelEntry gram dist
                = concatMap (\d -> concat [showGram gram, "\t", d, "\n"])
                            (showDist dist)

            showGram = intercalate "\t" . map showToken . toList

            showDist (Distribution _ dist)
                = map (\(t, c) -> showToken t ++ "\t" ++ show c)
                $ Map.assocs dist

            showToken Start     = "Start"
            showToken End       = "End"
            showToken (Token a) = show a

-- | Construct an n-gram model from a list of lists of grams. For NLP purposes
-- these units are likely to be words.
makeModel :: forall n a. (KnownNat n, 2 <= n, Ord a) => [[a]] -> Model n a
makeModel = mconcat . map (ngramsToModel . ngrams n)
    where n = Proxy :: Proxy n

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

-- | Generate a sequence of grams based on the probabilities described by
-- a given n-gram model.
generate :: forall n a. (KnownNat n, 2 <= n, Ord a) => Model n a -> RVar [a]
generate (Model model) = fromTokens <$> genFrom start
    where
        n = Proxy :: Proxy n
        start = Seq.fromList $ initial n

        genFrom current = do
            let Distribution size samples = model Map.! current
            let sampleList = Map.assocs samples
            let sampleListCDF = scanl1 (\(_, s) (t, c) -> (t, c + s)) sampleList
            index <- uniform 1 size
            let ((token, _):_) = dropWhile ((< index) . snd) sampleListCDF
            let nextGram = Seq.drop 1 current Seq.|> token
            if token == End
                then return []
                else (token :) <$> genFrom nextGram

        fromTokens []           = []
        fromTokens (Start:xs)   = fromTokens xs
        fromTokens (End:xs)     = fromTokens xs
        fromTokens (Token x:xs) = x : fromTokens xs
