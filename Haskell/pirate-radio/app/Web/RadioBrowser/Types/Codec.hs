{-# LANGUAGE OverloadedStrings #-}

module Web.RadioBrowser.Types.Codec ( Codec(..)
                                    ) where

import Data.Aeson (FromJSON(..), (.:), withObject)



data Codec = Codec { codecName         :: String
                   , codecStationCount :: Int
                   } deriving (Show)

instance FromJSON Codec where
  parseJSON = withObject "Codec" $ \codec -> do
    _codecName         <- codec .: "name"
    _codecStationCount <- codec .: "stationcount"

    pure Codec { codecName         = _codecName
               , codecStationCount = _codecStationCount }