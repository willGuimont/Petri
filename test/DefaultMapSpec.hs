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

type FunctorType = Maybe

spec :: Spec
spec = do
  describe "empty"
    $ do
      it "is null" $ property $ \(x :: ValueType) -> DM.null (DM.empty x)
  describe "singleton"
    $ do
      it "contains element"
        $ property
        $ \(def :: ValueType) (key :: KeyType) (value :: ValueType)
        -> key `DM.member` DM.singleton def key value
  describe "fromList"
    $ do
      it "contains elements"
        $ property
        $ \def (xs :: [(KeyType, ValueType)])
        -> all ((`DM.member` DM.fromList def xs) . fst) xs
  describe "insert"
    $ do
      it "contains element after insert"
        $ property
        $ \(k :: KeyType) (v :: ValueType) m -> k `DM.member` DM.insert k v m
  describe "adjust"
    $ do
      it "adjust value at index"
        $ property
        $ \(Fn (f :: ValueType -> ValueType)) (k :: KeyType) m
        -> DM.adjust f k m DM.! k `shouldBe` f (DM.lookup k m)
      it "does not modify other values"
        $ property
        $ \(Fn (f :: ValueType -> ValueType)) (k :: KeyType) m
        -> let g = filter ((/= k) . fst)
           in g (DM.toList (DM.adjust f k m)) `shouldBe` g (DM.toList m)
  describe "adjustWithKey"
    $ do
      it "adjust value at index"
        $ property
        $ \(Fn (fUncurried :: (KeyType, ValueType) -> ValueType)) k m
        -> let f = curry fUncurried
           in DM.adjustWithKey f k m DM.! k `shouldBe` f k (DM.lookup k m)
      it "does not modify other values"
        $ property
        $ \(Fn (fUncurried :: (KeyType, ValueType) -> ValueType)) (k :: KeyType) m
        -> let g = filter ((/= k) . fst)
               f = curry fUncurried
           in g (DM.toList (DM.adjustWithKey f k m)) `shouldBe` g (DM.toList m)
  describe "delete"
    $ do
      it "does not contain element after delete"
        $ property
        $ \k (m :: DM.DefaultMap KeyType ValueType)
        -> k `DM.notMember` DM.delete k m
  describe "lookup"
    $ do
      it "returns element when present"
        $ property
        $ \k v (m :: DM.DefaultMap KeyType ValueType)
        -> DM.lookup k (DM.insert k v m) `shouldBe` v
      it "returns default when not present"
        $ property
        $ \(def :: ValueType) (k :: KeyType)
        -> DM.empty def DM.! k `shouldBe` def
  describe "member"
    $ do
      it "returns true when in map"
        $ property
        $ \(def :: ValueType) (key :: KeyType) value
        -> key `DM.member` DM.singleton def key value
      it "returns false when not in map"
        $ property
        $ \m key -> key
        `DM.notMember` DM.delete key (m :: DM.DefaultMap KeyType ValueType)
  describe "null"
    $ do
      it "is null when empty"
        $ property
        $ \def -> DM.null (DM.empty def :: DM.DefaultMap KeyType ValueType)
      it "is not null when not empty"
        $ property
        $ \def (xs :: [(KeyType, ValueType)]) -> DM.null (DM.fromList def xs)
        `shouldBe` null xs
  describe "size"
    $ do
      it "returns size"
        $ property
        $ \def (xs :: [(KeyType, ValueType)]) -> DM.size (DM.fromList def xs)
        `shouldBe` length (nub (fmap fst xs))
  describe "toList"
    $ do
      it "returns as list"
        $ property
        $ \def (xs :: [(KeyType, ValueType)])
        -> let xs_nub = nubBy (\x y -> fst x == fst y) xs
           in DM.toList (DM.fromList def xs_nub)
              `shouldBe` sortBy (\x y -> compare (fst x) (fst y)) (nub xs_nub)
  describe "keys"
    $ do
      it "returns all keys"
        $ property
        $ \def (xs :: [(KeyType, ValueType)]) -> DM.keys (DM.fromList def xs)
        `shouldBe` (sort . nub) (fst <$> xs)
  describe "Functor"
    $ do
      it "preserves identity morphism"
        $ property
        $ \(m :: DM.DefaultMap KeyType ValueType) -> DM.toList (fmap id m)
        `shouldBe` DM.toList m
      it "preserves composition of morphisms" $ property functorCompositionProp
  describe "Applicative"
    $ do
      it "preserves identity morphism"
        $ property
        $ \(m :: DM.DefaultMap KeyType ValueType) -> (pure id <*> m)
        `shouldBe` m
      it "respects homomorphism" $ property applicativeHomomorphismProp
      it "preserves composition of morphisms"
        $ property applicativeCompositionProp
      it "respects interchange law" $ property applicativeInterchangeProp
      it "is consistent with fmap" $ property applicativeFunctorConsistentProp
  describe "Foldable"
    $ do
      it "fold" $ property foldableProp
  describe "Traversable"
    $ do
      it "traverse 1"
        $ traverse (\x -> Just x) (DM.fromList 0 [(0, 1), (1, 2), (2, 3)])
        `shouldBe` Just (DM.fromList 0 [(0, 1), (1, 2), (2, 3)])
      it "traverse 2"
        $ traverse
          (\x -> if x == 1
                 then Nothing
                 else Just x)
          (DM.fromList 0 [(0, 1), (1, 2), (2, 3)])
        `shouldBe` Nothing

functorCompositionProp :: DM.DefaultMap KeyType ValueType
                       -> Fun OtherType AnotherType
                       -> Fun ValueType OtherType
                       -> IO ()
functorCompositionProp m (Fn f) (Fn g) = fmap (f . g) m
  `shouldBe` (fmap f . fmap g) m

applicativeHomomorphismProp :: Fun ValueType OtherType -> ValueType -> IO ()
applicativeHomomorphismProp (Fn f) x = (pure f <*> m) `shouldBe` (pure (f x))
  where
    m :: DM.DefaultMap KeyType ValueType
    m = pure x

applicativeCompositionProp :: DM.DefaultMap KeyType (Fun OtherType AnotherType)
                           -> DM.DefaultMap KeyType (Fun ValueType OtherType)
                           -> DM.DefaultMap KeyType ValueType
                           -> IO ()
applicativeCompositionProp uWrapped vWrapped w = (pure (.) <*> u <*> v <*> w)
  `shouldBe` (u <*> (v <*> w))
  where
    u = fmap applyFun uWrapped

    v = fmap applyFun vWrapped

applicativeInterchangeProp
  :: DM.DefaultMap KeyType (Fun ValueType OtherType) -> ValueType -> IO ()
applicativeInterchangeProp uWrapped y = (u <*> pure y)
  `shouldBe` (pure ($ y) <*> u)
  where
    u = fmap applyFun uWrapped

applicativeFunctorConsistentProp
  :: Fun ValueType OtherType -> DM.DefaultMap KeyType ValueType -> IO ()
applicativeFunctorConsistentProp (Fn f) m = (fmap f m)
  `shouldBe` (pure f <*> m)

foldableProp :: Fun (ValueType, OtherType) OtherType
             -> [(KeyType, ValueType)]
             -> OtherType
             -> ValueType
             -> IO ()
foldableProp fWrapped xs z def = foldr f z (DM.fromList def xs)
  `shouldBe` foldr f z ((def:) $ snd <$> DM.toList (DM.fromList def xs))
  where
    f = curry $ applyFun fWrapped
