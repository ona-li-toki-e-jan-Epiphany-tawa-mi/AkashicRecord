{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.UUID.Types (fromString)
import Data.Maybe (fromJust)
import Web.RadioBrowser

printAll :: Show a => [a] -> IO ()
printAll [] = return ()
printAll (x:xs) = do
    print x
    printAll xs

main :: IO ()
main = do
    avalibleServers <- getAvailableServers
    case avalibleServers of
        Left  dnsError    -> print dnsError
        Right ipAddresses -> do
            let ipAddress = head ipAddresses
                {- uuid = fromJust $ fromString "451b1f03-636f-4a7a-ab1c-ba336e175d9e"
            stations <- getStationsByUUID ipAddress [uuid] -}
            --countries <- getCountries ipAddress (Just "C") Nothing
            stations <- getStationsByTag ipAddress "pirate" False Nothing
            --printAll countries
            printAll stations
