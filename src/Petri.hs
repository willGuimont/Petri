{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Petri
  ( Net,
    Id (Id),
    Place,
    Transition,
    PlaceDelta (PlaceDelta),
    emptyNet,
    addEmptyPlace,
    addEmptyTransition,
    placeDeltaOf,
    transitionAsDefaultMap,
    addPlaceDeltaToTransition,
    applyPlaceDeltaToNet,
    deltaOfTransition,
    applyPlaceDeltaToPlace,
    step,
    numTokenAtPlace,
  )
where

import Control.Lens
import qualified Data.Map as M
import Data.Monoid
import qualified DefaultMap as DM

newtype Id a = Id Integer
  deriving (Eq, Show)

deriving instance Ord (Id a)

-- Places
-- TODO add colored tokens
newtype Place = Place {_placeNumToken :: Integer} deriving (Show)

makeLenses ''Place

type Places = DM.DefaultMap (Id Place) Place

newtype PlaceDelta = PlaceDelta {_placeDeltaToken :: Integer}

makeLenses ''PlaceDelta

instance Show PlaceDelta where
  show = show . abs . view placeDeltaToken

instance Semigroup PlaceDelta where
  x <> y =
    PlaceDelta
      { _placeDeltaToken =
          view placeDeltaToken x + view placeDeltaToken y
      }

instance Monoid PlaceDelta where
  mempty = PlaceDelta {_placeDeltaToken = 0}

-- Transitions
-- TODO add transition priority
newtype Transition = Transition {_transMap :: DM.DefaultMap (Id Place) PlaceDelta} deriving (Show)

makeLenses ''Transition

type Arc = Place -> Maybe Place

-- Net
data Net = Net
  { _netPlaces :: Places,
    _netNextPlaceIndex :: Integer,
    _netTransitions :: M.Map (Id Transition) Transition,
    _netNextTransitionIndex :: Integer
  }
  deriving (Show)

makeLenses ''Net

-- Functions
emptyNet :: Net
emptyNet =
  Net
    { _netPlaces = DM.empty (Place {_placeNumToken = 0}),
      _netNextPlaceIndex = 0,
      _netTransitions = M.empty,
      _netNextTransitionIndex = 0
    }

emptyPlace :: Place
emptyPlace = Place {_placeNumToken = 0}

-- Add an empty place to the net, uses consecutive indices
addEmptyPlace :: Net -> (Id Place, Net)
addEmptyPlace n = (i', n')
  where
    i = n ^. netNextPlaceIndex
    i' = Id i
    n' =
      (netPlaces %~ DM.insert i' emptyPlace) . (netNextPlaceIndex %~ (+ 1)) $
        n

emptyTransition :: Transition
emptyTransition = Transition {_transMap = DM.empty (placeDeltaOf 0)}

deltaOfTransition :: Id Transition -> Id Place -> Net -> Maybe PlaceDelta
deltaOfTransition it ip n = DM.lookup ip . view transMap <$> M.lookup it nt
  where
    nt = n ^. netTransitions

transitionAsDefaultMap ::
  Net -> Id Transition -> Maybe (DM.DefaultMap (Id Place) PlaceDelta)
transitionAsDefaultMap n i =
  M.lookup i (n ^. netTransitions)
    ^? _Just . transMap

-- Add an empty transition to the net, uses consecutive indices
addEmptyTransition :: Net -> (Id Transition, Net)
addEmptyTransition n = (i', n')
  where
    i = n ^. netNextTransitionIndex
    i' = Id i :: Id Transition
    n' =
      (netTransitions %~ M.insert i' emptyTransition)
        . (netNextTransitionIndex %~ (+ 1))
        $ n

-- Creates a PlaceDelta
placeDeltaOf :: Integer -> PlaceDelta
placeDeltaOf x = PlaceDelta {_placeDeltaToken = x}

-- Mappend PlaceDelta to transition
addPlaceDeltaToTransition ::
  Id Place -> Id Transition -> PlaceDelta -> Net -> Net
addPlaceDeltaToTransition ip it pd =
  netTransitions . ix it
    %~ (transMap %~ DM.adjust (<> pd) ip)

-- Transformes a PlaceDelta to an Arc, so that we can apply it to Place
placeDeltaToArc :: PlaceDelta -> Arc
placeDeltaToArc pd = test . go
  where
    go = placeNumToken %~ (+ pd ^. placeDeltaToken)
    test x =
      if x ^. placeNumToken >= 0
        then Just x
        else Nothing

-- Apply a PlaceDelta to a Place
-- Returns Nothing if the delta would occur a negative value
applyPlaceDeltaToPlace :: PlaceDelta -> Place -> Maybe Place
applyPlaceDeltaToPlace = placeDeltaToArc

-- Apply a PlaceDelta to a particular Place in a Net
-- Returns Nothing if the delta would occur a negative value
applyPlaceDeltaToNet :: Id Place -> PlaceDelta -> Net -> Maybe Net
applyPlaceDeltaToNet i pd n = do
  let place = DM.lookup i $ n ^. netPlaces
  newPlace <- applyPlaceDeltaToPlace pd place
  return $ (netPlaces %~ DM.insert i newPlace) n

-- Returns Just is num token is non negative,
-- Returns Nothing if any place has negative value
-- Uses Applicative instance of DefaultMap so that
-- each PlaceDelta is applied to corresponding places
applyTransition :: Places -> Transition -> Maybe Places
applyTransition ps ts = sequenceA (arcs <*> ps)
  where
    tsDM = ts ^. transMap
    arcs = fmap applyPlaceDeltaToPlace tsDM

-- Try to apply the first fireable transition to the net
-- If none can fire, returns Nothing
-- TODO should sort by priority
step :: Net -> Maybe Net
step net = do
  nextNet <-
    getFirst . mconcat $
      fmap (First . applyTransition places) transitions
  return $ (netPlaces .~ nextNet) net
  where
    places = net ^. netPlaces
    transitions = snd <$> M.toList (net ^. netTransitions)

-- Returns the number of token at a place
numTokenAtPlace :: Id Place -> Net -> Integer
numTokenAtPlace p n = DM.lookup p (n ^. netPlaces) ^. placeNumToken
