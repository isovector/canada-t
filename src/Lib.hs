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

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Data.Bifunctor
import           Data.ByteString.Lazy (unpack)
import           Data.Char (chr)
import           Data.Foldable (for_)
import qualified Data.IntMap.Strict as IM
import           Data.List (sortBy, transpose, intercalate)
import           Data.Maybe (catMaybes)
import           Data.Ord
import           Parse
import           Prelude hiding (all)
import           Types


pad :: Int -> a -> [a] -> [a]
pad 0 _ _      = []
pad n f (a:as) = a : pad (n - 1) f as
pad n f []     = replicate n f


unboundedFloor :: (Floating a, Ord a) => a -> a -> Canada a
unboundedFloor l v = do
  guard $ l < v
  pure $ log (v - l) / log 10


newtype SumList a = SumList { unSumList :: ZipList a }
  deriving (Eq, Functor, Applicative, Foldable)

instance Show a => Show (SumList a) where
  show (SumList (ZipList a)) = intercalate "\t" $ fmap show a

instance (Num a, Ord a) => Ord (SumList a) where
  compare = comparing $ sum . unSumList


bigEnough :: Canada ()
bigEnough = do
  pop <- eh Population Total
  guard $ pop >= 10000


affordableCity :: Canada Float
affordableCity = do
  percentOfPopulation AffordableRenters


communicableCity :: Canada Float
communicableCity = do
  percentOfPopulation FrenchOnlySpeakers >>= pure . negate


-- score

weightedMetrics :: [(Canada Float, Float)]
weightedMetrics =
  [ (walkableCity,  0.5)
  , (dateableCity, 1.1)
  , (feminineCity, 0.3)
  , (technicalCity, 0.8)
  , (smartCity, 1.5)
  , (youngCity, 1.0)
  , (cheapCity, 2.0)
  , (culturalCity, 0.7)
  , (rightSizeCity, 1.2)
  , (affordableCity, 0.5)
  , (communicableCity, 1)
  ]


bestScore :: Float
bestScore = sum
           $ liftA2 (*)
                    (SumList (ZipList $ repeat 10))
                    weights


allMetrics :: Canada [Float]
allMetrics = do
  bigEnough
  traverse fst weightedMetrics

weights :: SumList Float
weights = SumList . ZipList $ fmap snd weightedMetrics



walkableCity :: Canada Float
walkableCity = do
  let walkWeight = 5
      busWeight  = 1
  walkable <- percentOf [BikingCommuters, WalkingCommuters] TotalCommuters
  busable  <- percentOf BusCommuters TotalCommuters
  let walkableScore = walkable * walkWeight + busable * busWeight
  pure walkableScore


ageStat :: [Stat]
ageStat = [ Population20'24
          , Population25'29
          , Population30'24
          ]


rightSizeCity :: Canada Float
rightSizeCity = do
  pop <- eh Population Total
  pure $ negate $ log $ abs (pop - 100000)



dateableCity :: Canada Float
dateableCity = do
  percSingle <- percentOfPopulation TotalUnmarried
  datingPool <- eh ageStat Female

  -- let percWomen = datingPool / ageGroup
  -- guard $ percWomen > 0.5

  myWomen <- unboundedFloor 0 $ datingPool * percSingle
  pure myWomen


feminineCity :: Canada Float
feminineCity = do
  datingPool <- eh ageStat Female
  ageGroup   <- eh ageStat Total
  pure $ datingPool / ageGroup


technicalCity :: Canada Float
technicalCity = do
  let sciScore = 4
      engScore = 1
      mathScore = 10

  sci  <- percentOfPopulation StudiedScience
  math <- percentOfPopulation StudiedMath
  eng  <- percentOfPopulation StudiedEngineering
  pure $ sci * sciScore
       + eng * engScore
       + math * mathScore


smartCity :: Canada Float
smartCity = do
  let bachScore = 1
      masterScore = 0.5
      phdScore = 3

  bach   <- percentOfPopulation HasBachelors
  master <- percentOfPopulation HasMasters
  phd    <- percentOfPopulation HasPhD
  pure $ bach * bachScore
       + master * masterScore
       + phd * phdScore


youngCity :: Canada Float
youngCity = do
  eh MedianAge Total >>= pure . negate


cheapCity :: Canada Float
cheapCity = do
  rent <- eh MedianRent Total
  -- guard $ rent < 900
  pure $ -rent


culturalCity :: Canada Float
culturalCity = do
  let musicScore  = 2
      artistScore = 1

  music <- percentOfPopulation StudiedMusic
  artist <- percentOfPopulation ProfessionalArtists
  pure $ music * musicScore
       + artist * artistScore


rounding :: RealFrac a => Int -> a -> a
rounding p a = fromInteger (round (10^p * a)) / 10^p


main :: IO ()
main = do
  csv <- loadCSV
  putStrLn "loaded yo"
  let known = fromEnum <$> [minBound @KnownCity .. maxBound]
      -- cs = filter (flip elem known . cityCode) $ getCities csv
      cs = getCities csv

  putStrLn "citied yo"
  let best = scoring allMetrics cs
      all = fmap snd best
      grouped = fmap minandmax $ transpose all
      normed = fmap (second $ zipWith normalize grouped) best
      best' = take 15 $ sortBy (flip $ comparing snd)
            $ fmap (second $ (liftA2 (*) weights) . SumList . ZipList) normed
  for_ best' $ \(city, vs) -> do
    let name = pad 15 ' '
             . fmap (chr . fromIntegral)
             . unpack
             $ cityName city
    putStr name
    putStr $ show (rounding 1 $ sum vs) ++ "\t"
    putStrLn $ show $ fmap (rounding 1) vs


normalize :: (Float, Float) -> Float -> Float
normalize (l, h) v =
  let z = (v - l) / (h - l) * 10
   in z


minandmax :: Ord a => [a] -> (a, a)
minandmax as = (minimum as, maximum as)


scoring :: (Ord a) => Canada a -> [City] -> [(City, a)]
scoring f cs = catMaybes $ fmap (\c -> fmap (c,) $ ohCanada c f) cs


hoistMaybe :: Maybe a -> Canada a
hoistMaybe = Canada . ReaderT . const
{-# INLINE hoistMaybe #-}


class IsStat a where
  getStatistic :: a -> Canada Statistic

instance IsStat Stat where
  getStatistic stat = do
    City{cityCode, cityData} <- Canada ask
    case IM.lookup (fromEnum stat) cityData of
      Just a -> pure a
      Nothing ->  error $ show stat ++ show cityCode
  {-# INLINE getStatistic #-}

instance IsStat [Stat] where
  getStatistic stats = do
    s <- traverse getStatistic stats
    pure $ sum s
  {-# INLINE getStatistic #-}


eh :: (IsStat s) => s -> Gender -> Canada Float
eh stat g = do
  Statistic{..}  <- getStatistic stat
  hoistMaybe $
    case g of
      Total  -> sTotal
      Male   -> sMale
      Female -> sFemale
{-# INLINE eh #-}


percentOfPopulation :: (IsStat s) => s -> Canada Float
percentOfPopulation s = percentOf s Population


percentOf :: (IsStat s1, IsStat s2) => s1 -> s2 -> Canada Float
percentOf s1 s2 = do
  n <- eh s1 Total
  d <- eh s2 Total
  pure $ n / d


inRange :: (Ord a) => a -> a -> a -> Canada ()
inRange l h v = guard $ l <= v && v <= h

