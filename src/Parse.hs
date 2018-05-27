{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall          #-}

module Parse where

import Data.List (groupBy)
import qualified Data.ByteString.Lazy as BS
import           Data.Csv
import           Data.Function
import qualified Data.IntMap.Strict as IM
import           Data.Maybe
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Types


buildStat :: Row -> (Int, Statistic)
buildStat = \Row{..} ->
  -- if S.member statKey set
  --    then Just
        ( statKey
        , Statistic
            (unpossible statTotal)
            (unpossible statMale)
            (unpossible statFemale)
        )
     -- else Nothing
  -- where
    -- set = [minBound .. maxBound]



buildCity :: [Row] -> City
buildCity rs =
  let Row{..} = head rs
   in City geoCode geoName . IM.fromList $ fmap buildStat rs


getCities :: Vector Row -> [City]
getCities = fmap buildCity . groupBy ((==) `on` geoCode). filter ((>= 1000) . geoCode) . V.toList


stripUtf8Bom :: BS.ByteString -> BS.ByteString
stripUtf8Bom bs = fromMaybe bs $ BS.stripPrefix "\239\187\191" bs


loadCSV :: IO (Vector Row)
loadCSV = do
  bs <- stripUtf8Bom <$> BS.readFile "census-more.csv"
  case decodeByName bs of
    Left err -> putStrLn err >> undefined
    Right res -> pure $ snd res



