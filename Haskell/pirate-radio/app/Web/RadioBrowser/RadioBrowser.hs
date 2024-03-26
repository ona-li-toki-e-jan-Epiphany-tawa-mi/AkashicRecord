{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.RadioBrowser.RadioBrowser ( getAvailableServers
                                     , IPAddress
                                     
                                     , getCountries
                                     , getCodecs
                                     , getStates
                                     , getLanguages
                                     , getTags

                                     , getAllStations
                                     , getStationsByUUID, getStationsByUUIDs
                                     , getStationsByName
                                     , getStationsByCodec
                                     , getStationsByCountry, getStationsByCountryCodeExact
                                     , getStationsByState
                                     , getStationsByLanguage
                                     , getStationsByTag
                                     , getStationsByURL
                                     ) where

import           Network.DNS.Lookup (lookupA)
import           Network.DNS (makeResolvSeed, defaultResolvConf, withResolver, DNSError)
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header (hUserAgent)
import qualified Data.ByteString as ByteString
import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as ByteStringUTF8
import           Data.UUID.Types (UUID, toASCIIBytes)
import           Web.RadioBrowser.Types ( Station, SearchParameters(..), appendSearchParameters, Country
                                        , Codec, State, URL, Language, Tag, emptySearchParameters )



type IPAddress = ByteString

getAvailableServers :: IO (Either DNSError [IPAddress])
getAvailableServers = do
  resolver <- makeResolvSeed defaultResolvConf
  response <- withResolver resolver $ \_resolver -> lookupA _resolver "all.api.radio-browser.info"

  case response of
    Left  dnsError    -> return $ Left dnsError
    Right ipAddresses -> return $ Right $ map (ByteStringUTF8.fromString . show) ipAddresses



baseRequest :: IPAddress -> Request
baseRequest serverIP = addRequestHeader hUserAgent "pirate-radio/0.1.0 In-Development"
                     $ setRequestMethod "GET"
                     $ setRequestSecure False
                     $ setRequestHost   serverIP
                       defaultRequest



validStandardSearchParameters :: Maybe SearchParameters -> Maybe SearchParameters
validStandardSearchParameters (Just (SearchParameters {..})) = Just
  emptySearchParameters { standardSortBy = standardSortBy
                        , reverseResults = reverseResults
                        , hideBroken     = hideBroken
                        , offset         = offset
                        , limit          = limit }
validStandardSearchParameters Nothing = Nothing

getCountries :: IPAddress -> Maybe ByteString -> Maybe SearchParameters -> IO [Country]
getCountries serverIP countryFilter searchParameters = do
  let request = appendSearchParameters (validStandardSearchParameters searchParameters)
              $ setRequestPath (ByteString.append "/json/countries"
                                                $ maybe "" (ByteString.append "/") countryFilter)
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Country])

getCodecs :: IPAddress -> Maybe ByteString -> Maybe SearchParameters -> IO [Codec]
getCodecs serverIP codecFilter searchParameters = do
  let request = appendSearchParameters (validStandardSearchParameters searchParameters)
              $ setRequestPath (ByteString.append "/json/codecs"
                                                $ maybe "" (ByteString.append "/") codecFilter)
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Codec])

getStates :: IPAddress
          -> Maybe ByteString
          -> Maybe ByteString
          -> Maybe SearchParameters
          -> IO [State]
getStates serverIP country stateFilter searchParameters = do
  let request = addToRequestQueryString (maybe [] (\_country -> [("country", Just _country)]) country)
              $ appendSearchParameters (validStandardSearchParameters searchParameters)
              $ setRequestPath (ByteString.append "/json/states"
                                                $ maybe "" (ByteString.append "/") stateFilter)
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [State])

getLanguages :: IPAddress -> Maybe ByteString -> Maybe SearchParameters -> IO [Language]
getLanguages serverIP languageFilter searchParameters = do
  let request = appendSearchParameters (validStandardSearchParameters searchParameters)
              $ setRequestPath (ByteString.append "/json/languages"
                                                $ maybe "" (ByteString.append "/") languageFilter)
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Language])

getTags :: IPAddress -> Maybe ByteString -> Maybe SearchParameters -> IO [Tag]
getTags serverIP tagFilter searchParameters = do
  let request = appendSearchParameters (validStandardSearchParameters searchParameters)
              $ setRequestPath (ByteString.append "/json/tags"
                                                $ maybe "" (ByteString.append "/") tagFilter)
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Tag])



getTermSeachPath :: ByteString -> Bool -> ByteString -> ByteString
getTermSeachPath requestPath findExact searchTerm =
  foldl1 ByteString.append [ requestPath
                           , if findExact then "exact/" else "/"
                           , searchTerm ]

validStationSearchParameters :: Maybe SearchParameters -> Maybe SearchParameters
validStationSearchParameters (Just (SearchParameters {..})) = Just
  emptySearchParameters { stationSortBy  = stationSortBy
                        , reverseResults = reverseResults
                        , hideBroken     = hideBroken
                        , offset         = offset
                        , limit          = limit }
validStationSearchParameters Nothing = Nothing

getAllStations :: IPAddress -> Maybe SearchParameters -> IO [Station]
getAllStations serverIP searchParameters = do
  let request = appendSearchParameters (validStationSearchParameters searchParameters)
              $ setRequestPath "/json/stations"
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Station])

getStationsByUUID :: IPAddress -> UUID -> Bool -> Maybe SearchParameters -> IO [Station]
getStationsByUUID serverIP uuid findExact searchParameters = do
  let request = appendSearchParameters (validStationSearchParameters searchParameters)
              $ setRequestPath (getTermSeachPath "/json/stations/byuuid"
                                                 findExact
                                               $ toASCIIBytes uuid)
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Station])

getStationsByUUIDs :: IPAddress -> [UUID] -> IO [Station]
getStationsByUUIDs serverIP uuids = do
  let request = setRequestPath "/json/stations/byuuid"
              $ addToRequestQueryString [("uuids", Just $ ByteString.intercalate ","
                                                        $ map toASCIIBytes uuids)]
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Station])

getStationsByName :: IPAddress -> String -> Bool -> Maybe SearchParameters -> IO [Station]
getStationsByName serverIP name findExact searchParameters = do
  let request = appendSearchParameters (validStationSearchParameters searchParameters)
              $ setRequestPath (getTermSeachPath "/json/stations/byname"
                                                 findExact
                                               $ ByteStringUTF8.fromString name)
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Station])

getStationsByCodec :: IPAddress -> String -> Bool -> Maybe SearchParameters -> IO [Station]
getStationsByCodec serverIP codec findExact searchParameters = do
  let request = appendSearchParameters (validStationSearchParameters searchParameters)
              $ setRequestPath (getTermSeachPath "/json/stations/bycodec"
                                                 findExact
                                               $ ByteStringUTF8.fromString codec)
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Station])

getStationsByCountry :: IPAddress -> String -> Bool -> Maybe SearchParameters -> IO [Station]
getStationsByCountry serverIP country findExact searchParameters = do
  let request = appendSearchParameters (validStationSearchParameters searchParameters)
              $ setRequestPath (getTermSeachPath "/json/stations/bycountry"
                                                 findExact
                                               $ ByteStringUTF8.fromString country)
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Station])

getStationsByCountryCodeExact :: IPAddress -> String -> Maybe SearchParameters -> IO [Station]
getStationsByCountryCodeExact serverIP countryCode searchParameters = do
  let request = appendSearchParameters (validStationSearchParameters searchParameters)
              $ setRequestPath (ByteString.append "/json/stations/bycountrycodeexact/"
                                                $ ByteStringUTF8.fromString countryCode)
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Station])

getStationsByState :: IPAddress -> String -> Bool -> Maybe SearchParameters -> IO [Station]
getStationsByState serverIP state findExact searchParameters = do
  let request = appendSearchParameters (validStationSearchParameters searchParameters)
              $ setRequestPath (getTermSeachPath "/json/stations/bystate"
                                                 findExact
                                               $ ByteStringUTF8.fromString state)
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Station])

getStationsByLanguage :: IPAddress -> String -> Bool -> Maybe SearchParameters -> IO [Station]
getStationsByLanguage serverIP language findExact searchParameters = do
  let request = appendSearchParameters (validStationSearchParameters searchParameters)
              $ setRequestPath (getTermSeachPath "/json/stations/bylanguage"
                                                 findExact
                                               $ ByteStringUTF8.fromString language)
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Station])

getStationsByTag :: IPAddress -> String -> Bool -> Maybe SearchParameters -> IO [Station]
getStationsByTag serverIP tag findExact searchParameters = do
  let request = appendSearchParameters (validStationSearchParameters searchParameters)
              $ setRequestPath (getTermSeachPath "/json/stations/bytag"
                                                 findExact
                                               $ ByteStringUTF8.fromString tag)
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Station])

getStationsByURL :: IPAddress -> URL -> IO [Station]
getStationsByURL serverIP url = do
  let request = setRequestPath "/json/stations/byurl"
              $ addToRequestQueryString [("url", Just $ ByteStringUTF8.fromString url)]
              $ baseRequest serverIP
  response <- httpJSON request

  return (getResponseBody response :: [Station])