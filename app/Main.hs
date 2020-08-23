{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Lens
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as M
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
type Position = (Float, Float)

type PlacePositions = M.Map (Id Place) Position

type TransitionPositions = M.Map (Id Transition) Position

data World = World { _worldNet :: Net
                   , _worldPlacePositions :: PlacePositions
                   , _worldTransitionPositions :: TransitionPositions
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

initialState :: World
initialState = World { _worldNet = testNet
                     , _worldPlacePositions = placePositionsTest
                     , _worldTransitionPositions = transitionPositionsTest
                     }

-- Draw
draw :: World -> Picture
draw w = pictures $ drawPlaces pp n ++ drawTransitions tp
  where
    pp = w ^. worldPlacePositions

    n = w ^. worldNet

    tp = w ^. worldTransitionPositions

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

    trans = translate x y $ rectangleWire transitionWidth transitionHeight

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
