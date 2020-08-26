module Main (main) where

import AppTypes
import Constants
import qualified Data.Map as M
import Draw
import Graphics.Gloss
import Inputs
import Petri

windowDisplay :: Display
windowDisplay = InWindow "Petri Net" (windowSize, windowSize) (10, 10)

initialState :: World
initialState =
  World
    { _worldNet = emptyNet,
      _worldPlacePositions = M.empty,
      _worldTransitionPositions = M.empty,
      _worldTransitionDirectionMap = M.empty,
      _worldPlacementMode = PlaceMode,
      _worldLastArcSelection = Nothing
    }

-- Update
update :: Float -> World -> World
update _ = id

-- Main
main :: IO ()
main =
  play windowDisplay (light orange) fps initialState draw inputHandler update
