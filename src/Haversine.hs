module Haversine (haversineDistance, Lat, Lng) where

import Control.Arrow ((***))
import Units (Km, Lat, Lng, Mi)

-- The latitude and longtitude are assumed to be in degrees.
haversineDistance :: (Lat, Lng) -> (Lat, Lng) -> IO (Maybe Mi)
haversineDistance a b = return $ Just mi
  where
    mi = kmToMi $ distDeg 6371 a b
    distDeg radius p1 p2 = distRad radius (deg2rad p1) (deg2rad p2)
    distRad radius (lat1, lng1) (lat2, lng2) =
      (2 * radius) *
      asin
        (min
          1.0
          (sqrt $
            haversine (lat2 - lat1) +
            ((cos lat1 * cos lat2) * haversine (lng2 - lng1))))
    deg2rad = d2r *** d2r
      where
        d2r = (/ 180) . (pi *)

kmToMi :: Km -> Mi
kmToMi = (* 0.621371)

-- The haversine of an angle
haversine :: Float -> Float
haversine = (^ 2) . sin . (/ 2)
