{-# LANGUAGE TemplateHaskell #-}

module AppTypes where

import           Control.Lens
import           Graphics.Gloss
import qualified Data.Map as M
import           Petri

type PlacePositions = M.Map (Id Place) Point

type TransitionPositions = M.Map (Id Transition) Point

data Direction = ToPlace
               | FromPlace

type TransitionDirections = M.Map (Id Place) Direction

type TransitionDirectionMap = M.Map (Id Transition) TransitionDirections

data PlacementMode = PlaceMode | TransitionMode | ArcMode

data World = World { _worldNet :: Net
                   , _worldPlacePositions :: PlacePositions
                   , _worldTransitionPositions :: TransitionPositions
                   , _worldTransitionDirectionMap :: TransitionDirectionMap
                   , _worldPlacementMode ::PlacementMode
                   }

makeLenses ''World