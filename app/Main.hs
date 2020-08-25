module Main (main) where

import AppTypes
import Constants
import Control.Lens
import qualified Data.Map as M
import Data.Maybe
import Draw
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Petri

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

windowDisplay :: Display
windowDisplay = InWindow "Petri Net" (windowSize, windowSize) (10, 10)

initialState :: World
initialState =
  World
    { _worldNet = emptyNet,
      _worldPlacePositions = M.empty,
      _worldTransitionPositions = M.empty,
      _worldTransitionDirectionMap = M.empty,
      _worldPlacementMode = PlaceMode
    }

-- Inputs
inputHandler :: Event -> World -> World
inputHandler (EventKey (Char ' ') Down _ _) w =
  (worldNet %~ (fromMaybe (w ^. worldNet) . step)) w
inputHandler (EventKey (Char 'z') Down _ _) w = (worldPlacementMode .~ PlaceMode) w
inputHandler (EventKey (Char 'x') Down _ _) w = (worldPlacementMode .~ TransitionMode) w
inputHandler (EventKey (Char 'c') Down _ _) w = (worldPlacementMode .~ ArcMode) w
inputHandler (EventKey (MouseButton LeftButton) Down _ pos) w = undefined
  where
inputHandler _ w = w

-- Update
update :: Float -> World -> World
update _ = id

-- Main
main :: IO ()
main =
  play windowDisplay (light orange) fps initialState draw inputHandler update
