{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.RadioBrowser.Types.SearchParameters ( SortStationBy(..)
                                               , StandardSearchSortBy(..)
                                               , SearchParameters(..)
                                               , emptySearchParameters
                                               , appendSearchParameters
                                               ) where

import           Network.HTTP.Simple (addToRequestQueryString, Request)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as ByteStringUTF8
import           Data.Maybe (catMaybes, isJust)



data SortStationBy = StationName | URL | Homepage | Favicon | Tags | StationCountry | StationState 
                   | StationLanguage | Votes | StationCodec | Bitrate | LastCheckOk | LastCheckTime 
                   | ClickTimeStamp | ClickCount | ClickTrend | ChangeTimeStamp | Random
                   deriving (Show)

data StandardSearchSortBy = StandardName | StationCount
                          deriving (Show)

queryForStationSort :: SortStationBy -> ByteString
queryForStationSort StationName     = "name"
queryForStationSort URL             = "url"
queryForStationSort Homepage        = "homepage"
queryForStationSort Favicon         = "favicon"
queryForStationSort Tags            = "tags"
queryForStationSort StationCountry  = "country"
queryForStationSort StationState    = "state"
queryForStationSort StationLanguage = "language"
queryForStationSort Votes           = "votes"
queryForStationSort StationCodec    = "codec"
queryForStationSort Bitrate         = "bitrate"
queryForStationSort LastCheckOk     = "lastcheckok"
queryForStationSort LastCheckTime   = "lastchecktime"
queryForStationSort ClickTimeStamp  = "clicktimestamp"
queryForStationSort ClickCount      = "clickcount"
queryForStationSort ClickTrend      = "clicktrend"
queryForStationSort ChangeTimeStamp = "changetimestamp"
queryForStationSort Random          = "random"

queryForStandardSearchSort :: StandardSearchSortBy -> ByteString
queryForStandardSearchSort StandardName    = "name"
queryForStandardSearchSort StationCount    = "stationcount"



data SearchParameters = SearchParameters { stationSortBy  :: Maybe SortStationBy
                                         , standardSortBy :: Maybe StandardSearchSortBy
                                         , reverseResults :: Maybe Bool
                                         , offset         :: Maybe Int
                                         , limit          :: Maybe Int
                                         , hideBroken     :: Maybe Bool
                                         }
                      deriving (Show)

emptySearchParameters :: SearchParameters
emptySearchParameters = SearchParameters { stationSortBy  = Nothing
                                         , standardSortBy = Nothing
                                         , reverseResults = Nothing
                                         , offset         = Nothing
                                         , limit          = Nothing
                                         , hideBroken     = Nothing }

data AdvancedStationSearchParameters = AdvancedStationSearchParameters 
  { name        :: Maybe ByteString, nameExact     :: Bool
  , country     :: Maybe ByteString, countryExact  :: Bool
  , countryCode :: Maybe ByteString
  , state       :: Maybe ByteString, stateExact    :: Bool
  , language    :: Maybe ByteString, languageExact :: Bool }



appendSearchParameters :: Maybe SearchParameters -> Request -> Request
appendSearchParameters searchParameters request 
  | null selectedParameters = request 
  | otherwise               = addToRequestQueryString selectedParameters request
  where selectedParameters = catMaybes $ generateStationSearchQueries searchParameters

generateStationSearchQueries :: Maybe SearchParameters -> [Maybe (ByteString, Maybe ByteString)]
generateStationSearchQueries Nothing          = []
generateStationSearchQueries (Just (SearchParameters {..})) =
  [ if isJust stationSortBy then 
      makeParameterQuery stationSortBy  "order" queryForStationSort
    else 
      makeParameterQuery standardSortBy "order" queryForStandardSearchSort
  , makeParameterQuery reverseResults "reverse"    boolToByteString
  , makeParameterQuery offset         "offset"     toByteString
  , makeParameterQuery limit          "limit"      toByteString
  , makeParameterQuery hideBroken     "hidebroken" boolToByteString ]

makeParameterQuery :: Maybe a
                   -> ByteString
                   -> (a -> ByteString)
                   -> Maybe (ByteString, Maybe ByteString)
makeParameterQuery (Just value) key valueTransformer = 
  Just (key, Just $ valueTransformer value)
makeParameterQuery Nothing _ _ = Nothing

toByteString :: Show a => a -> ByteString
toByteString = ByteStringUTF8.fromString . show

boolToByteString :: Bool -> ByteString
boolToByteString True  = "true"
boolToByteString False = "false"