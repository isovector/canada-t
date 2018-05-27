{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wall                   #-}

module Lib where

import Control.Exception (evaluate)
import Data.Maybe (catMaybes)
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Bifunctor
import           Data.Foldable (for_)
import qualified Data.IntMap.Strict as IM
import           Data.List (sortBy)
import           Data.Ord
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Parse
import           Types


unboundedFloor :: (Floating a, Ord a) => a -> a -> Query a
unboundedFloor l v = do
  guard $ l < v
  pure $ log (v - l) / log 10

unboundedCeil :: (Floating a, Ord a) => a -> a -> Query a
unboundedCeil u v = do
  guard $ v < u
  pure $ log (u - v) / log 10

unboundedDist :: (Floating a, Ord a) => a -> a -> a -> a -> Query a
unboundedDist x s m v = do
  pure $ clamp 0 x $ x - ((v - m) / s)^2


clamp :: (Num a, Ord a) => a -> a -> a -> a
clamp l h v = min h $ max l v

data SumList a = SumList { unSumList :: [a] }
  deriving (Eq, Functor)

instance Show a => Show (SumList a) where
  show (SumList a) = show a

instance (Num a, Ord a) => Ord (SumList a) where
  compare = comparing $ sum . unSumList

bestCity ::  Query (SumList Float)
bestCity = do
  let walkWeight = 5
      busWeight  = 1


  pop <- query Population Total -- >>= unboundedFloor 10000
  guard $ pop >= 10000

  let ageStat = [ Population20'24
                , Population25'29
                , Population30'24
                ]
  -- inRange 20000 450000 pop

  technical <- percentOfPopulation [StudiedScience, StudiedMath, StudiedEngineering]

  percSingle <- percentOfPopulation TotalUnmarried
  percSmart  <- percentOfPopulation HasBachelors
  datingPool <- query ageStat Female
  ageGroup   <- query ageStat Total

  let percWomen = datingPool / ageGroup
  -- guard $ percWomen > 0.5

  myAge <- query Population25'29 Female >>= unboundedFloor 0

  walkable <- percentOf [BikingCommuters, WalkingCommuters] TotalCommuters
  busable  <- percentOf BusCommuters TotalCommuters
  let walkableScore = walkable * walkWeight + busable * busWeight

  myWomen <- unboundedFloor 5000 $ datingPool * percSingle
  apts <- query [HighDensityApartments, LowDensityApartments] Total >>= unboundedFloor 0

  rent <- query MedianRent Total
  guard $ rent <= 1400

  let rent' = (1400 - rent) / 1400 * 5

  age <- query [MedianAge, AverageAge] Total >>= unboundedDist 3 20 (28 * 2)
  culture <- percentOfPopulation [StudiedMusic, ProfessionalArtists]

  pure $ fmap (rounding 2) $ SumList
    [ percWomen * 5
    , myWomen
    , rent'
    , culture * 100
    , walkableScore
    , age
    ]


rounding :: RealFrac a => Int -> a -> a
rounding p a = fromInteger (round (10^p * a)) / 10^p


main :: IO ()
main = do
  csv <- loadCSV
  putStrLn "loaded yo"
  cs <- evaluate $ getCities csv
  putStrLn "citied yo"
  let best = rankBy bestCity cs
  for_ best $ print . bimap cityName id


rankBy :: (Ord a) => Query a -> [City] -> [(City, a)]
rankBy f = sortBy (flip $ comparing snd)
         . catMaybes
         . fmap (\c -> fmap (c,) $ runQuery c f)
         -- . traverse (\c -> fmap (c,) <$> runQuery c f)


hoistMaybe :: Maybe a -> Query a
hoistMaybe = Query . ReaderT . const
{-# INLINE hoistMaybe #-}


class IsStat a where
  getStatistic :: a -> Query Statistic

instance IsStat Stat where
  getStatistic stat = do
    City{cityCode, cityData} <- Query ask
    case IM.lookup (fromEnum stat) cityData of
      Just a -> pure a
      Nothing ->  error $ show stat ++ show cityCode
  {-# INLINE getStatistic #-}

instance IsStat [Stat] where
  getStatistic stats = do
    s <- traverse getStatistic stats
    pure $ sum s
  {-# INLINE getStatistic #-}


query :: (IsStat s) => s -> Gender -> Query Float
query stat g = do
  Statistic{..}  <- getStatistic stat
  hoistMaybe $
    case g of
      Total  -> sTotal
      Male   -> sMale
      Female -> sFemale
{-# INLINE query #-}


percentOfPopulation :: (IsStat s) => s -> Query Float
percentOfPopulation s = percentOf s Population


percentOf :: (IsStat s1, IsStat s2) => s1 -> s2 -> Query Float
percentOf s1 s2 = do
  n <- query s1 Total
  d <- query s2 Total
  pure $ n / d


inRange :: (Ord a) => a -> a -> a -> Query ()
inRange l h v = guard $ l <= v && v <= h

