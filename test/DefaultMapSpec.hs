module DefaultMapSpec where

import Data.List
import qualified DefaultMap as DM
import Test.Hspec
import Test.QuickCheck
import Prelude

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (DM.DefaultMap k v) where
  arbitrary = do
    def <- arbitrary
    m <- arbitrary
    return $ DM.fromList def m

type KeyType = Integer

type ValueType = Char

spec :: Spec
spec = do
  describe "DefaultMap.empty" $ do
    it "is null" $
      property $
        \x -> DM.null (DM.empty (x :: ValueType))
  describe "DefaultMap.singleton" $ do
    it "contains element" $
      property $
        \def key value -> key `DM.member` (DM.singleton (def :: ValueType) (key :: KeyType) (value :: ValueType))
  describe "DefaultMap.fromList" $ do
    it "contains elements" $
      property $ \def xs -> all (`DM.member` (DM.fromList def xs)) (map fst (xs :: [(KeyType, ValueType)]))
  describe "DefaultMap.insert" $ do
    it "contains element after insert" $ do
      property $ \k v m -> k `DM.member` (DM.insert (k :: KeyType) (v :: ValueType) m)
  describe "DefaultMap.delete" $ do
    it "does not contain element after delete" $
      property $ \k m -> k `DM.notMember` (DM.delete (k :: KeyType) (m :: DM.DefaultMap KeyType ValueType))
  describe "DefaultMap.lookup" $ do
    it "returns element when present" $ do
      property $ \k v m -> v == (DM.lookup k (DM.insert (k :: KeyType) (v :: ValueType) m))
    it "returns default when not present" $ do
      property $ \def k -> def == DM.empty (def :: ValueType) DM.! (k :: KeyType)
  describe "DefaultMap.member" $ do
    it "returns true when in map" $
      property $ \def key value -> key `DM.member` (DM.singleton (def :: ValueType) (key :: KeyType) (value :: ValueType))
    it "returns false when not in map" $
      property $ \m key -> key `DM.notMember` (DM.delete key (m :: DM.DefaultMap KeyType ValueType))
  describe "DefaultMap.null" $ do
    it "is null when empty" $ do
      property $ \def -> DM.null ((DM.empty def) :: DM.DefaultMap KeyType ValueType)
    it "is not null when not empty" $
      property $ \def xs -> DM.null ((DM.fromList def xs) :: DM.DefaultMap KeyType ValueType) == null xs
  describe "DefaultMap.size" $ do
    it "returns size" $ do
      property $ \def xs -> DM.size ((DM.fromList def xs) :: DM.DefaultMap KeyType ValueType) == (length $ nub (fmap fst xs))
