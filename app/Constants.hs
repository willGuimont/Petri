module Constants where

fps :: Int
fps = 20

windowSize :: Int
windowSize = 1000

placeRadius :: Float
placeRadius = 50

numTokenScale :: Float
numTokenScale = 0.25

numTokenOffset :: Float
numTokenOffset = - placeRadius / 4

transitionWidth :: Float
transitionWidth = 25

transitionHeight :: Float
transitionHeight = 150

instructionPosition :: (Float, Float)
instructionPosition = (- fromIntegral windowSize / 2 + 10, - fromIntegral windowSize / 2 + 10)

instructionText :: String
instructionText = "Space to step, q to place places, w to place transitions, e to add arcs"

instructionTextScale :: Float
instructionTextScale = 0.2

arrowSize :: Float
arrowSize = 15

numDeltaTokenScale :: Float
numDeltaTokenScale = 0.25

placementModePosition :: (Float, Float)
placementModePosition = (- fromIntegral windowSize / 2 + 10, fromIntegral windowSize / 2 - 25)

placementModeScale :: Float
placementModeScale = 0.2
