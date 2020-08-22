{-# LANGUAGE ScopedTypeVariables #-}

module PetriSpec (spec) where

import           Test.Hspec
import qualified Petri as P

spec :: Spec
spec = do
  describe "simple"
    $ do
      it "should work"
        $ do
          -- Make net
          let net0 = P.emptyNet
          -- Add 2 place
          let (place1, net1) = P.addEmptyPlace net0
          let (place2, net2) = P.addEmptyPlace net1
          -- Add transition
          let (transition, net3) = P.addEmptyTransition net2
          -- Add PlaceDelta of (-1) for Place 1 in transition
          let net4 = P.addPlaceDeltaToTransition
                place1
                transition
                (P.placeDeltaOf (-1))
                net3
          -- Add PlaceDelta of (+1) for Place 2 in transition
          let net5 = P.addPlaceDeltaToTransition
                place2
                transition
                (P.placeDeltaOf 1)
                net4
          -- Add PlaceDelta of (+1) for Place 2 in transition
          let net6 = P.addPlaceDeltaToTransition
                place2
                transition
                (P.placeDeltaOf 1)
                net5
          -- Add PlaceDelta of (+1) for Place 2 in transition
          let net7 = P.addPlaceDeltaToTransition
                place2
                transition
                (P.placeDeltaOf 1)
                net6
          -- Add PlaceDelta of (-1) for Place 2 in transition
          let net8 = P.addPlaceDeltaToTransition
                place2
                transition
                (P.placeDeltaOf (-1))
                net7
          -- Add 2 tokens to Place 1
          let Just net9 = P.applyPlaceDeltaToNet place1 (P.placeDeltaOf 2) net8
          -- Add 1 token to Place 2
          let Just net10 =
                P.applyPlaceDeltaToNet place2 (P.placeDeltaOf 1) net9
          -- Step
          let Just net11 = P.step net10
          -- Asserts
          P.numTokenAtPlace place1 net11 `shouldBe` 1
          P.numTokenAtPlace place2 net11 `shouldBe` 3