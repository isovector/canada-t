{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wall                   #-}

module Lib where

import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as BS
import           Data.Csv
import           Data.Foldable (for_)
import           Data.Function
import qualified Data.IntMap.Strict as IM
import           Data.List (sortBy)
import           Data.Maybe
import           Data.Ord
import           Data.Ratio
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Types


main :: IO ()
main = do
  csv <- loadCSV
  let cs = getCities csv
  best <- take 10 <$> rankBy (percentOfPopulation StudiedMath) cs
  for_ best $ print . bimap cityName (fromRational @Float)




rankBy :: (Applicative m, Ord a) => QueryT m a -> Vector City -> m [(City, a)]
rankBy f = fmap (sortBy (flip $ comparing snd) . V.toList . V.mapMaybe id)
         . traverse (\c -> fmap (c,) <$> runQueryT c f)


hoistMaybe :: Applicative m => Maybe a -> QueryT m a
hoistMaybe = QueryT . ReaderT . const . MaybeT . pure


getStatistic :: Monad m => Stat -> QueryT m Statistic
getStatistic stat = do
  City{cityData} <- QueryT ask
  hoistMaybe $ IM.lookup (fromEnum stat) cityData


query :: Monad m => Stat -> Gender -> QueryT m Float
query stat g = do
  Statistic{..}  <- getStatistic stat
  hoistMaybe $
    case g of
      Total  -> sTotal
      Male   -> sMale
      Female -> sFemale


percentOfPopulation :: Monad m => Stat -> QueryT m Rational
percentOfPopulation stat = do
  n <- round <$> query stat Total
  d <- round <$> query Population Total
  pure $ n % d


buildStat :: Row -> (Int, Statistic)
buildStat Row{..} =
  ( statKey
  , Statistic
      statName
      (unpossible statTotal)
      (unpossible statMale)
      (unpossible statFemale)
  )


buildCity :: Vector Row -> City
buildCity rs =
  let Row{..} = V.head rs
   in City geoCode geoName . IM.fromList . V.toList $ fmap buildStat rs


getCities :: Vector Row -> Vector City
getCities = fmap buildCity . groupBy ((==) `on` geoCode)


stripUtf8Bom :: BS.ByteString -> BS.ByteString
stripUtf8Bom bs = fromMaybe bs $ BS.stripPrefix "\239\187\191" bs


groupBy :: (a -> a -> Bool) -> Vector a -> Vector (Vector a)
groupBy eq v
  | V.null v = V.empty
  | otherwise =
      let x        = V.head v
          xs       = V.tail v
          (ys, zs) = V.span (eq x) xs
       in V.cons (V.cons x ys) $ groupBy eq zs


loadCSV :: IO (Vector Row)
loadCSV = do
  bs <- stripUtf8Bom <$> BS.readFile "census.csv"
  case decodeByName bs of
    Left err -> putStrLn err >> undefined
    Right res -> pure $ snd res


someFunc :: IO ()
someFunc = putStrLn "someFunc"

