{-# LANGUAGE ScopedTypeVariables #-}

module DefaultMapSpec where

import           Data.List
import qualified DefaultMap as DM
import           Prelude
import           Test.Hspec
import           Test.QuickCheck

instance (Ord k, Arbitrary k, Arbitrary v)
  => Arbitrary (DM.DefaultMap k v) where
  arbitrary = do
    def <- arbitrary
    DM.fromList def <$> arbitrary

type KeyType = Integer

type ValueType = Char

type OtherType = String

type AnotherType = Bool

spec :: Spec
spec = do
  describe "DefaultMap.empty"
    $ do
      it "is null" $ property $ \(x :: ValueType) -> DM.null (DM.empty x)
  describe "DefaultMap.singleton"
    $ do
      it "contains element"
        $ property
        $ \(def :: ValueType) (key :: KeyType) (value :: ValueType)
        -> key `DM.member` DM.singleton def key value
  describe "DefaultMap.fromList"
    $ do
      it "contains elements"
        $ property
        $ \def (xs :: [(KeyType, ValueType)])
        -> all ((`DM.member` DM.fromList def xs) . fst) xs
  describe "DefaultMap.insert"
    $ do
      it "contains element after insert"
        $ property
        $ \(k :: KeyType) (v :: ValueType) m -> k `DM.member` DM.insert k v m
  describe "DefaultMap.delete"
    $ do
      it "does not contain element after delete"
        $ property
        $ \k (m :: DM.DefaultMap KeyType ValueType)
        -> k `DM.notMember` DM.delete k m
  describe "DefaultMap.lookup"
    $ do
      it "returns element when present"
        $ property
        $ \k v (m :: DM.DefaultMap KeyType ValueType) -> v
        `shouldBe` DM.lookup k (DM.insert k v m)
      it "returns default when not present"
        $ property
        $ \(def :: ValueType) (k :: KeyType)
        -> def `shouldBe` DM.empty def DM.! k
  describe "DefaultMap.member"
    $ do
      it "returns true when in map"
        $ property
        $ \(def :: ValueType) (key :: KeyType) value
        -> key `DM.member` DM.singleton def key value
      it "returns false when not in map"
        $ property
        $ \m key -> key
        `DM.notMember` DM.delete key (m :: DM.DefaultMap KeyType ValueType)
  describe "DefaultMap.null"
    $ do
      it "is null when empty"
        $ property
        $ \def -> DM.null (DM.empty def :: DM.DefaultMap KeyType ValueType)
      it "is not null when not empty"
        $ property
        $ \def (xs :: [(KeyType, ValueType)]) -> DM.null (DM.fromList def xs)
        `shouldBe` null xs
  describe "DefaultMap.size"
    $ do
      it "returns size"
        $ property
        $ \def (xs :: [(KeyType, ValueType)]) -> DM.size (DM.fromList def xs)
        `shouldBe` length (nub (fmap fst xs))
  describe "DefaultMap.toList"
    $ do
      it "returns as list"
        $ property
        $ \def (xs :: [(KeyType, ValueType)])
        -> let xs_nub = nubBy (\x y -> fst x == fst y) xs
           in DM.toList (DM.fromList def xs_nub)
              `shouldBe` sortBy (\x y -> compare (fst x) (fst y)) (nub xs_nub)
  describe "DefaultMap.Functor"
    $ do
      it "preserves identity morphism"
        $ property
        $ \(m :: DM.DefaultMap KeyType ValueType) -> DM.toList (fmap id m)
        `shouldBe` DM.toList m
      it "preserves composition of morphisms"
        $ property
        $ \(m :: DM.DefaultMap KeyType ValueType) (Fn (f :: OtherType -> AnotherType)) (Fn (g :: ValueType -> OtherType))
        -> DM.toList (fmap (f . g) m) `shouldBe` DM.toList ((fmap f . fmap g) m)
