module MathUtils where

import Data.Bifunctor
import Graphics.Gloss

addPoints :: Point -> Point -> Point
addPoints p1 = bimap (fst p1 +) (snd p1 +)

subPoints :: Point -> Point -> Point
subPoints p1 = bimap (fst p1 -) (snd p1 -)

scalePoint :: Float -> Point -> Point
scalePoint x = bimap (* x) (* x)

normalizePoint :: Point -> Point
normalizePoint p = scalePoint (1 / n) p
  where
    n = normPoint p

normPoint :: Point -> Float
normPoint p = n
  where
    p2 = bimap (* fst p) (* snd p) p
    n = sqrt . uncurry (+) $ p2

isPointInCircle :: Point -> Float -> Point -> Bool
isPointInCircle p r pos = normPoint (subPoints p pos) < r

isPointInRectangle :: Point -> Float -> Float -> Point -> Bool
isPointInRectangle p w h pos = uncurry (&&) $ bimap (\x -> - hw <= x && x <= hw) (\x -> - hh <= x && x <= hh) $ subPoints pos p
  where
    hw = w / 2
    hh = h / 2
