{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall          #-}

module Parse where

import qualified Data.ByteString.Lazy as BS
import           Data.Csv
import           Data.Function
import qualified Data.IntMap.Strict as IM
import           Data.Maybe
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Types


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



