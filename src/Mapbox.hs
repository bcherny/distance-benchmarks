{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mapbox (mapboxDistance) where

import Data.Aeson (FromJSON(..), ToJSON(..), decode, encode, object, Value(Object), (.=), (.:), withObject)
import qualified Data.ByteString.Lazy.Char8 as L8
import Debug.Trace (trace)
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Units (Lat, Lng, Meters, Mi)

key = "pk.eyJ1IjoiYmNoZXJueSIsImEiOiJjaWd6cGdseWoweDNwd3ltMGhsenI1d2tvIn0.jzRreSEiv5JLGK2DcHyuug"

mapboxDistance :: (Lat, Lng) -> (Lat, Lng) -> IO (Maybe Mi)
mapboxDistance a b =
  metersToMiles . flattenResponse . deserialize . processResponse <$> rawDistance a b

rawDistance :: (Lat, Lng) -> (Lat, Lng) -> IO (Response L8.ByteString)
rawDistance (a, b) (c, d) = do
  manager <- newManager tlsManagerSettings
  httpLbs request manager
  where
    request = parseRequest_ url
    url = "https://api.mapbox.com/directions/v5/mapbox/cycling/" ++ (show b) ++ "," ++ (show a) ++ ";" ++ (show d) ++ "," ++ (show c) ++ "?access_token=" ++ key

-- TODO: use a typeclass
metersToMiles :: Maybe Meters -> Maybe Mi
metersToMiles Nothing = Nothing
metersToMiles (Just a) = Just (a * 0.000621371 :: Mi)

-- TODO: avoid Maybes
deserialize :: Maybe L8.ByteString -> Maybe MapboxDirectionsResponse
deserialize Nothing = Nothing
deserialize (Just raw) = decode raw

-- TODO: avoid Maybes
flattenResponse :: Maybe MapboxDirectionsResponse -> Maybe Meters
flattenResponse Nothing = Nothing
flattenResponse (Just a) =
  Just $ distance $ ((legs route) !! 0)
  where
    route = (routes a) !! 0

-- TODO: avoid Maybes
processResponse :: Response L8.ByteString -> Maybe L8.ByteString
processResponse response =
  case isFailure response of
    True -> trace ("HTTP error: " ++ show response) Nothing
    False -> Just $ responseBody response

isFailure :: Response L8.ByteString -> Bool
isFailure response
  | (statusCode (responseStatus response)) == 200 = False
  | otherwise = True

------------- types -------------

data MapboxDirectionsResponse = MapboxDirectionsResponse {
  routes :: [MapboxRoute]
} deriving (Show, Generic)
instance FromJSON MapboxDirectionsResponse
instance ToJSON MapboxDirectionsResponse

data MapboxRoute = MapboxRoute {
  legs :: [MapboxLeg]
} deriving (Show, Generic)
instance FromJSON MapboxRoute
instance ToJSON MapboxRoute

data MapboxLeg = MapboxLeg {
  distance :: Meters
} deriving (Show, Generic)
instance FromJSON MapboxLeg
instance ToJSON MapboxLeg
