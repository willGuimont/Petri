module Main (main) where

import AppTypes
import Constants
import qualified Data.Map as M
import Draw
import Graphics.Gloss.Interface.IO.Game
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
update :: Float -> World -> IO World
update _ = pure

-- Main
main :: IO ()
main =
  playIO windowDisplay (light orange) fps initialState draw inputHandler update
