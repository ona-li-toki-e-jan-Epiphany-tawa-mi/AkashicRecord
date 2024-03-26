{-# LANGUAGE OverloadedStrings #-}

module Web.RadioBrowser.Types.Country ( Country(..)
                                      ) where

import Data.Aeson (FromJSON(..), (.:), withObject)



data Country = Country { countryName         :: String
                       , iso3166_1           :: String
                       , countryStationCount :: Int
                       } deriving (Show)

instance FromJSON Country where
  parseJSON = withObject "Country" $ \country -> do
    _countryName         <- country .: "name"
    _iso3166_1           <- country .: "iso_3166_1"
    _countryStationCount <- country .: "stationcount"

    pure Country { countryName         = _countryName
                 , iso3166_1           = _iso3166_1
                 , countryStationCount = _countryStationCount }