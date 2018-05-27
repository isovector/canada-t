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


unboundedFloor :: (Monad m, Floating a, Ord a) => a -> a -> QueryT m a
unboundedFloor l v = do
  guard $ l < v
  pure $ log (v - l) / log 10

unboundedCeil :: (Monad m, Floating a, Ord a) => a -> a -> QueryT m a
unboundedCeil u v = do
  guard $ v < u
  pure $ log (u - v) / log 10

unboundedDist :: (Monad m, Floating a, Ord a) => a -> a -> a -> QueryT m a
unboundedDist x m v = do
  pure $ x - (log (abs $ m - v) / log 10)


data SumList a = SumList { unSumList :: [a] }
  deriving (Eq, Functor)

instance Show a => Show (SumList a) where
  show (SumList a) = show a

instance (Num a, Ord a) => Ord (SumList a) where
  compare = comparing $ sum . unSumList

bestCity :: Monad m => QueryT m (SumList Float)
bestCity = do
  let walkWeight = 5
      busWeight  = 1


  pop <- query Population Total

  let ageStat = [ Population20'24
                , Population25'29
                , Population30'24
                ]
  -- inRange 20000 200000 pop

  percSingle <- percentOfPopulation TotalUnmarried
  percSmart  <- percentOfPopulation HasBachelors
  datingPool <- query ageStat Female
  ageGroup <- query ageStat Total

  let percWomen = datingPool / ageGroup

  myAge <- query Population25'29 Female >>= unboundedFloor 0

  walkable <- percentOf [BikingCommuters, WalkingCommuters] TotalCommuters
  busable  <- percentOf BusCommuters TotalCommuters
  let walkableScore = walkable * walkWeight + busable * busWeight

  myWomen <- unboundedFloor 0 $ datingPool * percSingle
  apts <- query [HighDensityApartments, LowDensityApartments] Total >>= unboundedFloor 0

  rent <- query MedianRent Total >>= unboundedCeil 1200
  age <- query MedianAge Total >>= unboundedDist 3 28
  culture <- percentOfPopulation [StudiedMusic, ProfessionalArtists]

  pure $ fmap (rounding 2) $ SumList
    [ percWomen
    ]


rounding :: RealFrac a => Int -> a -> a
rounding p a = fromInteger (round (10^p * a)) / 10^p


main :: IO ()
main = do
  csv <- loadCSV
  let cs = getCities csv
  best <- take 10 <$> rankBy bestCity cs
  for_ best $ print . bimap cityName id


rankBy :: (Applicative m, Ord a) => QueryT m a -> Vector City -> m [(City, a)]
rankBy f = fmap (sortBy (flip $ comparing snd) . V.toList . V.mapMaybe id)
         . traverse (\c -> fmap (c,) <$> runQueryT c f)


hoistMaybe :: Applicative m => Maybe a -> QueryT m a
hoistMaybe = QueryT . ReaderT . const . MaybeT . pure


class IsStat a where
  getStatistic :: Monad m => a -> QueryT m Statistic

instance IsStat Stat where
  getStatistic stat = do
    City{cityData} <- QueryT ask
    hoistMaybe $ IM.lookup (fromEnum stat) cityData

instance IsStat [Stat] where
  getStatistic stats = do
    s <- traverse getStatistic stats
    pure $ sum s


query :: (Monad m, IsStat s) => s -> Gender -> QueryT m Float
query stat g = do
  Statistic{..}  <- getStatistic stat
  hoistMaybe $
    case g of
      Total  -> sTotal
      Male   -> sMale
      Female -> sFemale


percentOfPopulation :: (Monad m, IsStat s) => s -> QueryT m Float
percentOfPopulation s = percentOf s Population


percentOf :: (Monad m, IsStat s1, IsStat s2) => s1 -> s2 -> QueryT m Float
percentOf s1 s2 = do
  n <- query s1 Total
  d <- query s2 Total
  pure $ n / d


inRange :: (Monad m, Ord a) => a -> a -> a -> QueryT m ()
inRange l h v = guard $ l <= v && v <= h

