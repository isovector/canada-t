{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -Wall                   #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Types where

import           Control.Applicative ((<|>), Alternative ())
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Data.ByteString.Lazy (ByteString)
import           Data.Csv
import qualified Data.IntMap.Strict as IM
import           GHC.Generics (Generic)


newtype Canada a = Canada
  { unCanada :: ReaderT City Maybe a
  } deriving (Functor, Applicative, Monad, Alternative, MonadPlus)


ohCanada :: City -> Canada a -> Maybe a
ohCanada c (Canada q) = runReaderT q c


data Gender = Total | Male | Female
  deriving (Eq, Ord, Show, Bounded, Enum)


data KnownCity
  = Vancouver    -- 5915022
  | Terrace      -- 5949011
  | London       -- 3539036
  | Waterloo     -- 3530016
  | Mississauga  -- 3521005
  | Gatineau     -- 2481017
  | Ottawa       -- 3506008
  deriving (Eq, Show, Ord, Bounded)

instance Enum KnownCity where
  enumFromTo a b = filter (\x -> a <= x && x <= b) cities
    where
      cities = [Vancouver, Terrace, London, Waterloo, Mississauga, Gatineau, Ottawa]

  fromEnum Vancouver   = 5915022
  fromEnum Terrace     = 5949011
  fromEnum London      = 3539036
  fromEnum Waterloo    = 3530016
  fromEnum Mississauga = 3521005
  fromEnum Gatineau    = 2481017
  fromEnum Ottawa      = 3506008
  toEnum = error "nope"


data Stat
  = Population              -- 8
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
  | FrenchOnlySpeakers      -- 102
  | MedianIncomeAfterTaxes  -- 746
  | Immigrants              -- 1139
  | HouseOwners             -- 1618
  | Renters                 -- 1619
  | AffordableRenters       -- 1668
  | MedianRent              -- 1681
  | AverageRent             -- 1682
  | HasBachelors            -- 1692
  | HasMasters              -- 1696
  | HasPhD                  -- 1697
  | StudiedMusic            -- 1717
  | StudiedHumanities       -- 1720
  | StudiedSocialSciences   -- 1729
  | StudiedScience          -- 1741
  | StudiedMath             -- 1747
  | StudiedEngineering      -- 1752
  | IsEmployee              -- 1882
  | IsSelfEmployed          -- 1883
  | Professionals           -- 1911
  | ProfessionalArtists     -- 1916
  | TotalCommuters          -- 1930
  | BusCommuters            -- 1933
  | WalkingCommuters        -- 1934
  | BikingCommuters         -- 1935
  | CommuteTime0'14         -- 1938
  | CommuteTime15'29        -- 1939
  | FreshMigrants           -- 2234
  | RecentMigrants          -- 2243
  deriving (Eq, Show, Bounded)

instance Enum Stat where
  fromEnum Population             = 8
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
  fromEnum FrenchOnlySpeakers     = 102
  fromEnum MedianIncomeAfterTaxes = 746
  fromEnum Immigrants             = 1139
  fromEnum HouseOwners            = 1618
  fromEnum Renters                = 1619
  fromEnum AffordableRenters      = 1668
  fromEnum MedianRent             = 1681
  fromEnum AverageRent            = 1682
  fromEnum HasBachelors           = 1692
  fromEnum HasMasters             = 1696
  fromEnum HasPhD                 = 1697
  fromEnum StudiedMusic           = 1717
  fromEnum StudiedHumanities      = 1720
  fromEnum StudiedSocialSciences  = 1729
  fromEnum StudiedScience         = 1741
  fromEnum StudiedMath            = 1747
  fromEnum StudiedEngineering     = 1752
  fromEnum IsEmployee             = 1882
  fromEnum IsSelfEmployed         = 1883
  fromEnum Professionals          = 1911
  fromEnum ProfessionalArtists    = 1916
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
  { geoCode    :: !Int
  , geoName    :: ByteString
  , statKey    :: !Int
  , statTotal  :: !(Possibly Float)
  , statMale   :: !(Possibly Float)
  , statFemale :: !(Possibly Float)
  } deriving (Generic, Show)


data Statistic = Statistic
  { sTotal  :: !(Maybe Float)
  , sMale   :: !(Maybe Float)
  , sFemale :: !(Maybe Float)
  } deriving (Show)

liftNum :: (Float -> Float -> Float) -> Statistic -> Statistic -> Statistic
liftNum f s1 s2 = Statistic
  { sTotal  = f <$> sTotal  s1 <*> sTotal  s2
  , sMale   = f <$> sMale   s1 <*> sMale   s2
  , sFemale = f <$> sFemale s1 <*> sFemale s2
  }

instance Num Statistic where
  (+)           = liftNum (+)
  (*)           = liftNum (*)
  (-)           = liftNum (-)
  abs           = error "abs"
  signum        = error "signum"
  fromInteger a = Statistic (Just $ fromInteger a)
                            (Just $ fromInteger a)
                            (Just $ fromInteger a)

instance Fractional Statistic where
  (/)            = liftNum (/)
  fromRational a = Statistic (Just $ fromRational a)
                             (Just $ fromRational a)
                             (Just $ fromRational a)


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

