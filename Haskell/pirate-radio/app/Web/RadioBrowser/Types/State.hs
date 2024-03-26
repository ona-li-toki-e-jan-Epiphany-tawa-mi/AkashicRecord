{-# LANGUAGE OverloadedStrings #-}

module Web.RadioBrowser.Types.State ( State(..)
                                    ) where

import Data.Aeson (FromJSON(..), (.:), withObject)



data State = State { stateName         :: String
                   , stateCountry      :: String
                   , stateStationCount :: Int
                   } deriving (Show)

instance FromJSON State where
  parseJSON = withObject "State" $ \state -> do
    _stateName         <- state .: "name"
    _stateCountry      <- state .: "country"
    _stateStationCount <- state .: "stationcount"

    pure State { stateName         = _stateName
               , stateCountry      = _stateCountry
               , stateStationCount = _stateStationCount }