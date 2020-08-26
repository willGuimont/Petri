{-# LANGUAGE TemplateHaskell #-}

module AppTypes where

import Control.Lens
import qualified Data.Map as M
import Graphics.Gloss
import Petri

type PlacePositions = M.Map (Id Place) Point

type TransitionPositions = M.Map (Id Transition) Point

data Direction
  = ToPlace
  | FromPlace
  deriving (Show)

type TransitionDirections = M.Map (Id Place) Direction

type TransitionDirectionMap = M.Map (Id Transition) TransitionDirections

data PlacementMode
  = PlaceMode
  | TransitionMode
  | ArcMode
  deriving (Show)

data Selection
  = PlaceSelection (Id Place)
  | TransitionSelection (Id Transition)
  deriving (Show)

data World = World
  { _worldNet :: Net,
    _worldPlacePositions :: PlacePositions,
    _worldTransitionPositions :: TransitionPositions,
    _worldTransitionDirectionMap :: TransitionDirectionMap,
    _worldPlacementMode :: PlacementMode,
    _worldLastArcSelection :: Maybe Selection
  } deriving (Show)

makeLenses ''World
