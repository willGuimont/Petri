module PointUtils where

import           Data.Bifunctor
import           Graphics.Gloss

addPoints :: Point -> Point -> Point
addPoints p1 = bimap (fst p1 +) (snd p1 +)

subPoints :: Point -> Point -> Point
subPoints p1 = bimap (fst p1 -) (snd p1 -)

scalePoint :: Float -> Point -> Point
scalePoint x = bimap (* x) (* x)

normalizePoint :: Point -> Point
normalizePoint p = scalePoint (1 / n) p
  where
    p2 = bimap (* fst p) (* snd p) p

    n = sqrt $ uncurry (+) p2