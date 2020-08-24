{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Lens
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Data.Tuple
import qualified Data.Map as M
import qualified DefaultMap as DM
import           Petri

{-
Workflow for UI

0 . Create Net
1 . Click to add a Place at position (x1, y1): Place 1
2 . Click to add a Place at position (x2, y2): Place 2
3 . Click to add a Transition at position (x3, y3): Transistion 1
4 . Drag from Place 1 to Transition 1: PlaceDelta of (-1) for Place 1 in Transition 1, show an arc from Place 1 to Transition 1
5 . Drag from Transition 1 to Place 2: PlaceDelta of (+1) for Place 2 in Transition 1, show an arc from Transition 1 to Place 2
5 . Click arc from Transition 1 to Place 2, modify PlaceDelta for Place 2 to (+2)
6 . Drag from Transition 1 to Place 2: PlaceDelta for Place 2 is (+3)
7 . Shift click arc from Transition 1 to Place 2, modify PlaceDelta for Place 2 to (+2)
8 . Click Place 1 to add a token
9 . Click Place 1 to add a token
10. Click Place 2 to add a token
11. Click Play, Transition 1 fires, Place 1 is now empty, Place 2 has 2 tokens

-}
-- Type
type PlacePositions = M.Map (Id Place) Point

type TransitionPositions = M.Map (Id Transition) Point

data Direction = ToPlace
               | FromPlace

type TransitionDirections = M.Map (Id Place) Direction

type TransitionDirectionMap = M.Map (Id Transition) TransitionDirections

data World = World { _worldNet :: Net
                   , _worldPlacePositions :: PlacePositions
                   , _worldTransitionPositions :: TransitionPositions
                   , _worldTransitionDirectionMap :: TransitionDirectionMap
                   }

makeLenses ''World

-- Constants
fps :: Int
fps = 20

windowSize :: Int
windowSize = 500

placeRadius :: Float
placeRadius = 50

numTokenScale :: Float
numTokenScale = 0.25

numTokenOffset :: Float
numTokenOffset = -placeRadius / 4

transitionWidth :: Float
transitionWidth = 25

transitionHeight :: Float
transitionHeight = 150

stepButtonPosition :: (Float, Float)
stepButtonPosition = (190, -210)

stepButtonText :: String
stepButtonText = "Step"

stepButtonTextScale :: Float
stepButtonTextScale = 0.25

stepButtonSize :: (Float, Float)
stepButtonSize = (100, 50)

arrowSize :: Float
arrowSize = 15

numDeltaTokenScale = 0.25

-- Utils
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

-- Game
windowDisplay :: Display
windowDisplay = InWindow "Petri Net" (windowSize, windowSize) (10, 10)

testNet :: Net
testNet = net11
  where
    net0 = emptyNet

    (place1, net1) = addEmptyPlace net0

    (place2, net2) = addEmptyPlace net1

    (transition, net3) = addEmptyTransition net2

    net4 = addPlaceDeltaToTransition place1 transition (placeDeltaOf (-1)) net3

    net5 = addPlaceDeltaToTransition place2 transition (placeDeltaOf 1) net4

    net6 = addPlaceDeltaToTransition place2 transition (placeDeltaOf 1) net5

    net7 = addPlaceDeltaToTransition place2 transition (placeDeltaOf 1) net6

    net8 = addPlaceDeltaToTransition place2 transition (placeDeltaOf (-1)) net7

    Just net9 = applyPlaceDeltaToNet place1 (placeDeltaOf 2) net8

    Just net10 = applyPlaceDeltaToNet place2 (placeDeltaOf 1) net9

    Just net11 = step net10

placePositionsTest :: PlacePositions
placePositionsTest = M.fromList [(Id 0, (-150, 0)), (Id 1, (150, 0))]

transitionPositionsTest :: TransitionPositions
transitionPositionsTest = M.fromList [(Id 0, (0, 0))]

transitionDirectionMapTest :: TransitionDirectionMap
transitionDirectionMapTest =
  M.fromList [(Id 0, M.fromList [(Id 0, FromPlace), (Id 1, ToPlace)])]

initialState :: World
initialState = World { _worldNet = testNet
                     , _worldPlacePositions = placePositionsTest
                     , _worldTransitionPositions = transitionPositionsTest
                     , _worldTransitionDirectionMap = transitionDirectionMapTest
                     }

-- Draw
draw :: World -> Picture
draw w = pictures
  $ drawPlaces pp n
  ++ drawTransitions tp
  ++ drawButtons
  ++ drawArcsForTransitions tp pp tdm n
  where
    pp = w ^. worldPlacePositions

    n = w ^. worldNet

    tp = w ^. worldTransitionPositions

    tdm = w ^. worldTransitionDirectionMap

drawPlaces :: PlacePositions -> Net -> [Picture]
drawPlaces pp n = concat [drawPlace pp n i | i <- placeIndices]
  where
    placeIndices = M.keys pp

drawPlace :: PlacePositions -> Net -> Id Place -> [Picture]
drawPlace pp n i = [placePicture, numTokenText]
  where
    Just (x, y) = M.lookup i pp

    numToken = numTokenAtPlace i n

    translated = translate x y

    placePicture = translated $ color black $ circle placeRadius

    -- TODO draw dots instead?
    numTokenText = translated
      $ translate numTokenOffset numTokenOffset
      $ scale numTokenScale numTokenScale
      $ text
      $ show numToken

drawTransitions :: TransitionPositions -> [Picture]
drawTransitions tp = concat [drawTransition tp i | i <- transitionIndices]
  where
    transitionIndices = M.keys tp

drawTransition :: TransitionPositions -> Id Transition -> [Picture]
drawTransition tp i = [trans]
  where
    Just (x, y) = M.lookup i tp

    trans = translate x y $ rectangleSolid transitionWidth transitionHeight

drawButtons :: [Picture]
drawButtons = [stepText, stepBoundingBox]
  where
    translated = uncurry translate stepButtonPosition

    stepText = translated
      $ translate (-45) (-10)
      $ scale stepButtonTextScale stepButtonTextScale
      $ text stepButtonText

    stepBoundingBox = translated $ uncurry rectangleWire stepButtonSize

drawArcsForTransitions
  :: TransitionPositions
  -> PlacePositions
  -> TransitionDirectionMap
  -> Net
  -> [Picture]
drawArcsForTransitions tp pp tdm n =
  concat [drawArcForTransition tp pp tdm i n | i <- transitionIndices]
  where
    transitionIndices = M.keys tp

drawArcForTransition
  :: TransitionPositions
  -> PlacePositions
  -> TransitionDirectionMap
  -> Id Transition
  -> Net
  -> [Picture]
drawArcForTransition tp pp tdm i n =
  concat [drawArc tp pp td i ip n | ip <- placeIndices]
  where
    placeIndices = maybe [] DM.keys (transitionAsDefaultMap n i)

    Just td = M.lookup i tdm

drawArc :: TransitionPositions
        -> PlacePositions
        -> TransitionDirections
        -> Id Transition
        -> Id Place
        -> Net
        -> [Picture]
drawArc tp pp td it ip n = numTokenText:arrow:[line [lineStart, transPos]]
  where
    Just transPos = M.lookup it tp

    Just placePos = M.lookup ip pp

    dp = normalizePoint $ subPoints transPos placePos

    startOffset = placeRadius

    lineStart = addPoints placePos $ scalePoint startOffset dp

    center = scalePoint 0.5 $ addPoints lineStart transPos

    translated = uncurry translate center

    angle = (* (-180 / pi)) $ uncurry atan2 $ swap dp

    Just arrowDirection = M.lookup ip td

    dir :: Picture -> Picture
    trans :: Picture -> Picture
    (dir, trans) = case arrowDirection of
      ToPlace   -> (scale (-1) (-1), translate (-arrowSize) 0)
      FromPlace -> (scale 1 1, id)

    rotated = rotate angle

    arrowLegs = line [(-1, -1), (0, 0), (-1, 1)]

    arrow = translated $ rotated $ trans $ dir $ scale arrowSize arrowSize arrowLegs

    Just numToken = deltaOfTransition it ip n

    numTokenText = translated $ translate 0 arrowSize $ scale numDeltaTokenScale numDeltaTokenScale $ text $ show numToken

-- Inputs
inputHandler :: Event -> World -> World
inputHandler _ = id

-- Update
update :: Float -> World -> World
update _ = id

-- Main
main :: IO ()
main =
  play windowDisplay (light orange) fps initialState draw inputHandler update
