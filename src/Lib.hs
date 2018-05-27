{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Lib where

import           Control.Applicative ((<|>), Alternative ())
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Csv
import           Data.Function
import qualified Data.IntMap.Strict as IM
import           Data.Maybe
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           GHC.Generics (Generic)


newtype QueryT m a = QueryT
  { unQueryT :: ReaderT City (MaybeT m) a
  } deriving (Functor, Applicative, Monad, Alternative, MonadPlus)


data Gender = Total | Male | Female
  deriving (Eq, Ord, Show, Bounded, Enum)


hoistMaybe :: Applicative m => Maybe a -> QueryT m a
hoistMaybe = QueryT . ReaderT . const . MaybeT . pure


query :: Monad m => Int -> Gender -> QueryT m Float
query key g = do
  City{cityData} <- QueryT ask
  Statistic{..}  <- hoistMaybe $ IM.lookup key cityData
  hoistMaybe $
    case g of
      Total  -> sTotal
      Male   -> sMale
      Female -> sFemale


data Row = Row
  { geoCode    :: Int
  , geoName    :: ByteString
  , statName   :: ByteString
  , statKey    :: Int
  , statTotal  :: Possibly Float
  , statMale   :: Possibly Float
  , statFemale :: Possibly Float
  } deriving (Generic, Show)


data Statistic = Statistic
  { sName   :: ByteString
  , sTotal  :: Maybe Float
  , sMale   :: Maybe Float
  , sFemale :: Maybe Float
  } deriving (Show)


data City = City
  { cityCode :: Int
  , cityName :: ByteString
  , cityData :: IM.IntMap Statistic
  } deriving (Show)


instance FromNamedRecord Row


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


newtype Possibly a = Possibly { unpossible :: Maybe a }
  deriving (Eq, Ord, Show, Read, Functor, Applicative, Alternative)

instance FromField a => FromField (Possibly a) where
  parseField f = (Possibly . Just <$> parseField f)
             <|> (pure $ Possibly Nothing)


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

