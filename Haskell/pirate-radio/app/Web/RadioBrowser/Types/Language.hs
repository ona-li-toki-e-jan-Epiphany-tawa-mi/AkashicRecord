{-# LANGUAGE OverloadedStrings #-}

module Web.RadioBrowser.Types.Language ( Language(..)
                                       ) where

import Data.Aeson (FromJSON(..), (.:), withObject, (.:?))
import Data.Maybe (fromMaybe)



data Language = Language { languageName         :: String
                         , iso639               :: String
                         , languageStationCount :: Int
                         } deriving (Show)

instance FromJSON Language where
  parseJSON = withObject "Language" $ \language -> do
    _languageName         <- language .:  "name"
    _iso639               <- language .:? "iso_639"
    _languageStationCount <- language .:  "stationcount"

    pure Language { languageName         = _languageName
                  , iso639               = fromMaybe "" _iso639
                  , languageStationCount = _languageStationCount }