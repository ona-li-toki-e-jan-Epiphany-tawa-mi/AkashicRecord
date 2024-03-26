{-# LANGUAGE OverloadedStrings #-}

module Web.RadioBrowser.Types.Tag ( Tag(..)
                                  ) where

import Data.Aeson (FromJSON(..), (.:), withObject)



data Tag = Tag { tagName         :: String
               , tagStationCount :: Int
               } deriving (Show)

instance FromJSON Tag where
  parseJSON = withObject "Tag" $ \tag -> do
    _tagName         <- tag .: "name"
    _tagStationCount <- tag .: "stationcount"

    pure Tag { tagName         = _tagName
             , tagStationCount = _tagStationCount }