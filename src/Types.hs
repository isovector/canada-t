{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# OPTIONS_GHC -Wall            #-}

module Types where

import           Control.Applicative ((<|>), Alternative ())
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.ByteString.Lazy (ByteString)
import           Data.Csv
import qualified Data.IntMap.Strict as IM
import           Data.Monoid ((<>))
import           GHC.Generics (Generic)


newtype QueryT m a = QueryT
  { unQueryT :: ReaderT City (MaybeT m) a
  } deriving (Functor, Applicative, Monad, Alternative, MonadPlus)


runQueryT :: City -> QueryT m a -> m (Maybe a)
runQueryT c (QueryT q) = runMaybeT $ runReaderT q c


data Gender = Total | Male | Female
  deriving (Eq, Ord, Show, Bounded, Enum)


data Stat
  = Population              -- 1
  | Population2011          -- 2
  | Population20'24         -- 15
  | Population25'29         -- 16
  | Population30'24         -- 17
  | PercentChild            -- 35
  | PercentAdult            -- 36
  | AverageAge              -- 39
  | MedianAge               -- 40
  | HighDensityApartments   -- 43
  | LowDensityApartments    -- 48
  | TotalMarried            -- 59
  | TotalUnmarried          -- 63
  | MedianIncomeAfterTaxes  -- 746
  | Immigrants              -- 1139
  | HouseOwners             -- 1618
  | Renters                 -- 1619
  | MedianRent              -- 1681
  | AverageRent             -- 1682
  | HasBachelors            -- 1692
  | HasMasters              -- 1696
  | HasPhD                  -- 1697
  | StudiedHumanities       -- 1720
  | StudiedSocialSciences   -- 1729
  | StudiedScience          -- 1741
  | StudiedMath             -- 1747
  | StudiedEngineering      -- 1752
  | IsEmployee              -- 1882
  | IsSelfEmployed          -- 1883
  | Professionals           -- 1911
  | TotalCommuters          -- 1930
  | BusCommuters            -- 1933
  | WalkingCommuters        -- 1934
  | BikingCommuters         -- 1935
  | CommuteTime0'14         -- 1938
  | CommuteTime15'29        -- 1939
  | FreshMigrants           -- 2234
  | RecentMigrants          -- 2243
  deriving (Eq, Show)

instance Enum Stat where
  fromEnum Population             = 1
  fromEnum Population2011         = 2
  fromEnum Population20'24        = 15
  fromEnum Population25'29        = 16
  fromEnum Population30'24        = 17
  fromEnum PercentChild           = 35
  fromEnum PercentAdult           = 36
  fromEnum AverageAge             = 39
  fromEnum MedianAge              = 40
  fromEnum HighDensityApartments  = 43
  fromEnum LowDensityApartments   = 48
  fromEnum TotalMarried           = 59
  fromEnum TotalUnmarried         = 63
  fromEnum MedianIncomeAfterTaxes = 746
  fromEnum Immigrants             = 1139
  fromEnum HouseOwners            = 1618
  fromEnum Renters                = 1619
  fromEnum MedianRent             = 1681
  fromEnum AverageRent            = 1682
  fromEnum HasBachelors           = 1692
  fromEnum HasMasters             = 1696
  fromEnum HasPhD                 = 1697
  fromEnum StudiedHumanities      = 1720
  fromEnum StudiedSocialSciences  = 1729
  fromEnum StudiedScience         = 1741
  fromEnum StudiedMath            = 1747
  fromEnum StudiedEngineering     = 1752
  fromEnum IsEmployee             = 1882
  fromEnum IsSelfEmployed         = 1883
  fromEnum Professionals          = 1911
  fromEnum TotalCommuters         = 1930
  fromEnum BusCommuters           = 1933
  fromEnum WalkingCommuters       = 1934
  fromEnum BikingCommuters        = 1935
  fromEnum CommuteTime0'14        = 1938
  fromEnum CommuteTime15'29       = 1939
  fromEnum FreshMigrants          = 2234
  fromEnum RecentMigrants         = 2243
  toEnum = error "i didn't write to enum"


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

liftNum :: (Float -> Float -> Float) -> Statistic -> Statistic -> Statistic
liftNum f s1 s2 = Statistic
  { sName   = buildName (sName s1) (sName s2)
  , sTotal  = f <$> sTotal  s1 <*> sTotal  s2
  , sMale   = f <$> sMale   s1 <*> sMale   s2
  , sFemale = f <$> sFemale s1 <*> sFemale s2
  }

buildName :: ByteString -> ByteString -> ByteString
buildName n1 n2 = n1 <> "+" <> n2

instance Num Statistic where
  (+)         = liftNum (+)
  (*)         = liftNum (*)
  (-)         = liftNum (-)
  abs         = error "abs"
  signum      = error "signum"
  fromInteger = error "fromInteger"

instance Fractional Statistic where
  (/)          = liftNum (/)
  fromRational = error "fromRational"


data City = City
  { cityCode :: Int
  , cityName :: ByteString
  , cityData :: IM.IntMap Statistic
  } deriving (Show)


instance FromNamedRecord Row


newtype Possibly a = Possibly { unpossible :: Maybe a }
  deriving (Eq, Ord, Show, Read, Functor, Applicative, Alternative)

instance FromField a => FromField (Possibly a) where
  parseField f = (Possibly . Just <$> parseField f)
             <|> (pure $ Possibly Nothing)

