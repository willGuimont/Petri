module Inputs (inputHandler) where

import AppTypes
import Constants
import Control.Lens
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import MathUtils
import Petri
import System.Random

-- TODO handle deletion of places and transitions
inputHandler :: Event -> World -> IO World
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) w = do
  gen <- newStdGen
  pure $ (worldNet %~ (fromMaybe (w ^. worldNet) . step gen)) w
inputHandler (EventKey (Char 'q') Down _ _) w = pure $ changeMode PlaceMode w
inputHandler (EventKey (Char 'w') Down _ _) w = pure $ changeMode TransitionMode w
inputHandler (EventKey (Char 'e') Down _ _) w = pure $ (resetSelection . changeMode ArcMode) w
inputHandler (EventKey (MouseButton LeftButton) Down _ pos) w = pure $ placeStuff (w ^. worldPlacementMode) pos w
inputHandler _ w = pure w

changeMode :: PlacementMode -> World -> World
changeMode pm = worldPlacementMode .~ pm

placeStuff :: PlacementMode -> Point -> World -> World
placeStuff PlaceMode pos w = fromMaybe addedPlace $ mPlaceId >>= addTokenToPlace >>= \x -> Just ((worldNet .~ x) w)
  where
    n = w ^. worldNet
    (placeId, n') = addEmptyPlace n
    addedPlace = (worldPlacePositions %~ M.insert placeId pos) . (worldNet .~ n') $ w
    mPlaceId = findSelectionInPlaces w pos
    addTokenToPlace (PlaceSelection i) = applyPlaceDeltaToNet i (placeDeltaOf 1) n
    addTokenToPlace _ = Nothing
placeStuff TransitionMode pos w = w'
  where
    (transId, n') = addEmptyTransition $ w ^. worldNet
    w' = (worldTransitionDirectionMap %~ M.insert transId M.empty) . (worldTransitionPositions %~ M.insert transId pos) . (worldNet .~ n') $ w
placeStuff ArcMode pos w = handleSelection (findSelection w pos) w

handleSelection :: Maybe Selection -> World -> World
handleSelection Nothing w = resetSelection w
handleSelection (Just newSelection) w = go lastSelection newSelection
  where
    lastSelection = w ^. worldLastArcSelection
    go :: Maybe Selection -> Selection -> World
    go Nothing _ = (worldLastArcSelection ?~ newSelection) w
    go (Just (PlaceSelection ip)) (TransitionSelection it) = resetSelection $ addArcBetween ip it FromPlace w
    go (Just (TransitionSelection it)) (PlaceSelection ip) = resetSelection $ addArcBetween ip it ToPlace w
    go _ _ = resetSelection w

addArcBetween :: Id Place -> Id Transition -> Direction -> World -> World
addArcBetween ip it d = (worldNet %~ addPlaceDeltaToTransition ip it (placeDeltaOf $ delta d)) . (worldTransitionDirectionMap %~ addDirectionToMap)
  where
    delta FromPlace = -1
    delta ToPlace = 1
    addDirectionToMap :: TransitionDirectionMap -> TransitionDirectionMap
    addDirectionToMap = ix it %~ M.insert ip d

resetSelection :: World -> World
resetSelection = worldLastArcSelection .~ Nothing

findSelection :: World -> Point -> Maybe Selection
findSelection w pos = getFirst $ mconcat $ First . (\f -> f w pos) <$> [findSelectionInPlaces, findSelectionInTransitions]

findSelectionInPlaces :: World -> Point -> Maybe Selection
findSelectionInPlaces w pos = listToMaybe collisionPlaces
  where
    places = M.toList $ w ^. worldPlacePositions
    collisionPlaces = PlaceSelection . fst <$> filter (\(_, p) -> isPointInCircle p placeRadius pos) places

findSelectionInTransitions :: World -> Point -> Maybe Selection
findSelectionInTransitions w pos = listToMaybe collisionTransition
  where
    transitions = M.toList $ w ^. worldTransitionPositions
    collisionTransition = TransitionSelection . fst <$> filter (\(_, p) -> isPointInRectangle p transitionWidth transitionHeight pos) transitions
