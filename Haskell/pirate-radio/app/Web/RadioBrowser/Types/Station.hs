{-# LANGUAGE OverloadedStrings #-}

module Web.RadioBrowser.Types.Station ( Date, URL
                                      , Station(..)
                                      ) where

import Data.Aeson (FromJSON(..), (.:), withObject, (.:?))
import Data.UUID.Types (UUID)
import Data.List.Split (splitOn)                        



type Date = String
type URL  = String

data Station = Station { changeUUID         :: UUID
                       , stationUUID        :: UUID
                       , stationName        :: String
                       , url                :: URL, resolvedURL :: URL
                       , homepage           :: URL
                       , favicon            :: URL
                       , tags               :: [String]
                       , countryCodes       :: [String]
                       , stationState       :: String
                       , languages          :: [String], languageCodes :: [String]
                       , votes              :: Int
                       , lastChangeTime     :: Date, lastChangeTimeISO8601 :: Date
                       , codec              :: String
                       , bitrate            :: Int
                       , hls                :: Bool
                       , lastCheckOk        :: Bool
                       , lastCheckTime      :: Date, lastCheckTimeISO8601      :: Date
                       , lastCheckOkTime    :: Date, lastCheckOkTimeISO8601    :: Date
                       , lastLocalCheckTime :: Date, lastLocalCheckTimeISO8601 :: Date
                       , clickTimestamp     :: Date, clickTimestampISO8601     :: Date
                       , clickCount         :: Int
                       , clickTrend         :: Int
                       , sslError           :: Bool
                       , geoLatitude        :: Maybe Double, geoLongitude :: Maybe Double
                       , hasExtendedInfo    :: Bool
                       } deriving Show

instance FromJSON Station where
  parseJSON = withObject "Station" $ \station -> do
    _changeUUID                <- station .:  "changeuuid"
    _stationUUID               <- station .:  "stationuuid"
    _stationName               <- station .:  "name"
    _url                       <- station .:  "url"
    _resolvedURL               <- station .:  "url_resolved"
    _homepage                  <- station .:  "homepage"
    _favicon                   <- station .:  "favicon"
    _tags                      <- station .:  "tags"
    _countryCodes              <- station .:  "countrycode"
    _stationState              <- station .:  "state"
    _languages                 <- station .:  "language"
    _languageCodes             <- station .:  "languagecodes"
    _votes                     <- station .:  "votes"
    _lastChangeTime            <- station .:  "lastchangetime"
    _lastChangeTimeISO8601     <- station .:  "lastchangetime_iso8601"
    _codec                     <- station .:  "codec"
    _bitrate                   <- station .:  "bitrate"
    _hls                       <- station .:  "hls"
    _lastCheckOk               <- station .:  "lastcheckok"
    _lastCheckTime             <- station .:  "lastchecktime"
    _lastCheckTimeISO8601      <- station .:  "lastchecktime_iso8601"
    _lastCheckOkTime           <- station .:  "lastcheckoktime"
    _lastCheckOkTimeISO8601    <- station .:  "lastcheckoktime_iso8601"
    _lastLocalCheckTime        <- station .:  "lastlocalchecktime"
    _lastLocalCheckTimeISO8601 <- station .:? "lastlocalchecktime_iso8601"
    _clickTimestamp            <- station .:  "clicktimestamp"
    _clickTimestampISO8601     <- station .:  "clicktimestamp_iso8601"
    _clickCount                <- station .:  "clickcount"
    _clickTrend                <- station .:  "clicktrend"
    _sslError                  <- station .:  "ssl_error"
    _geoLatitude               <- station .:? "geo_lat"
    _geoLongitude              <- station .:? "geo_long"
    _hasExtendedInfo           <- station .:  "has_extended_info"

    pure Station { changeUUID                = _changeUUID
                 , stationUUID               = _stationUUID
                 , stationName               = _stationName
                 , url                       = _url
                 , resolvedURL               = _resolvedURL
                 , homepage                  = _homepage
                 , favicon                   = _favicon
                 , tags                      = splitOnComma _tags
                 , countryCodes              = splitOnComma _countryCodes
                 , stationState              = _stationState
                 , languages                 = splitOnComma _languages
                 , languageCodes             = splitOnComma _languageCodes
                 , votes                     = _votes
                 , lastChangeTime            = _lastChangeTime
                 , lastChangeTimeISO8601     = _lastChangeTimeISO8601
                 , codec                     = _codec
                 , bitrate                   = _bitrate
                 , hls                       = isTruthy _hls
                 , lastCheckOk               = isTruthy _lastCheckOk
                 , lastCheckTime             = _lastCheckTime
                 , lastCheckTimeISO8601      = _lastCheckTimeISO8601
                 , lastCheckOkTime           = _lastCheckOkTime
                 , lastCheckOkTimeISO8601    = _lastCheckOkTimeISO8601
                 , lastLocalCheckTime        = _lastLocalCheckTime
                 , lastLocalCheckTimeISO8601 = emptyIfNull _lastLocalCheckTimeISO8601
                 , clickTimestamp            = _clickTimestamp
                 , clickTimestampISO8601     = _clickTimestampISO8601
                 , clickCount                = _clickCount
                 , clickTrend                = _clickTrend
                 , sslError                  = isTruthy _sslError
                 , geoLatitude               = _geoLatitude
                 , geoLongitude              = _geoLongitude
                 , hasExtendedInfo           = _hasExtendedInfo }
    where isTruthy :: Int -> Bool
          isTruthy 0 = False
          isTruthy _ = True

          emptyIfNull :: Maybe String -> String
          emptyIfNull Nothing       = ""
          emptyIfNull (Just string) = string

          splitOnComma :: String -> [String]
          splitOnComma = splitOn ","