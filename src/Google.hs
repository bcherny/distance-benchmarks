{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Google (googleDistance) where

import Data.Aeson (FromJSON(..), ToJSON(..), decode, encode, object, Value(Object), (.=), (.:), withObject)
import qualified Data.ByteString.Lazy.Char8 as L8
import Debug.Trace (trace)
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Units (Lat, Lng, Meters, Mi)

key = "AIzaSyBrWxU1O3Lfdl9s42-Rd4bRri4kHwi8B90"

googleDistance :: (Lat, Lng) -> (Lat, Lng) -> IO (Maybe Mi)
googleDistance a b =
  metersToMiles . flattenResponse . deserialize . processResponse <$> rawDistance a b

rawDistance :: (Lat, Lng) -> (Lat, Lng) -> IO (Response L8.ByteString)
rawDistance (a, b) (c, d) = do
  manager <- newManager tlsManagerSettings
  httpLbs request manager
  where
    request = parseRequest_ url
    url = "https://maps.googleapis.com/maps/api/directions/json?destination=" ++ (show a) ++ "," ++ (show b) ++ "&origin=" ++ (show c) ++ "," ++ (show d) ++ "&key=" ++ key

-- TODO: use a typeclass
metersToMiles :: Maybe Meters -> Maybe Mi
metersToMiles Nothing = Nothing
metersToMiles (Just a) = Just (a * 0.000621371 :: Mi)

-- TODO: avoid Maybes
deserialize :: Maybe L8.ByteString -> Maybe DirectionsResponse
deserialize Nothing = Nothing
deserialize (Just raw) = decode raw

-- TODO: avoid Maybes
flattenResponse :: Maybe DirectionsResponse -> Maybe Meters
flattenResponse Nothing = Nothing
flattenResponse (Just a) =
  Just $ value $ distance $ ((legs route) !! 0)
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

data DirectionsResponse = DirectionsResponse {
  routes :: [Route]
} deriving (Show, Generic)
instance FromJSON DirectionsResponse
instance ToJSON DirectionsResponse

data Route = Route {
  legs :: [Leg]
} deriving (Show, Generic)
instance FromJSON Route
instance ToJSON Route

data Leg = Leg {
  distance :: Distance
} deriving (Show, Generic)
instance FromJSON Leg
instance ToJSON Leg

data Distance = Distance {
  text :: String,
  value :: Meters
} deriving (Show, Generic)
instance FromJSON Distance
instance ToJSON Distance
