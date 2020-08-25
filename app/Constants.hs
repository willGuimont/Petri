module Constants where

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

instructionPosition :: (Float, Float)
instructionPosition = (-240, -240)

instructionText :: String
instructionText = "Space to step, z to place places, x to place transitions, c to add arcs"

instructionTextScale :: Float
instructionTextScale = 0.1

stepButtonSize :: (Float, Float)
stepButtonSize = (100, 50)

arrowSize :: Float
arrowSize = 15

numDeltaTokenScale :: Float
numDeltaTokenScale = 0.25