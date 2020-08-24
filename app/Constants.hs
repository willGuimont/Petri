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

stepButtonPosition :: (Float, Float)
stepButtonPosition = (190, -210)

stepButtonText :: String
stepButtonText = "Step"

stepButtonTextScale :: Float
stepButtonTextScale = 0.25

stepButtonSize :: (Float, Float)
stepButtonSize = (100, 50)

arrowSize :: Float
arrowSize = 15

numDeltaTokenScale :: Float
numDeltaTokenScale = 0.25