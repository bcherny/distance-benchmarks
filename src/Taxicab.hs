module Taxicab (taxicabDistance) where

import Units (Lat, Lng, Mi)

taxicabDistance :: (Lat, Lng) -> (Lat, Lng) -> Mi
taxicabDistance (a, b) (c, d) =
  (latDiffToMi latDiff) + (lngDiffToMi a lngDiff)
  where latDiff = (diff a c)
        lngDiff = (diff b d)

latDiffToMi :: Lat -> Mi
latDiffToMi = (* 69)

lngDiffToMi :: Lat -> Lng -> Mi
lngDiffToMi a b = b * (cos a)

diff :: (Num a, Ord a) => a -> a -> a
diff a b =
  case compare a' b' of
    LT -> b' - a'
    GT -> a' - b'
    EQ -> 0
  where a' = abs a
        b' = abs b