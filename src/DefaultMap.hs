module DefaultMap
    ( DefaultMap
    , empty
    , singleton
    , fromList
    , insert
    , delete
    , lookup
    , (!)
    , member
    , notMember
    , null
    , size
    , toList
    , keys) where

import qualified Data.Map as M
import           Data.Maybe
import           Prelude hiding (lookup, null)

data DefaultMap k v = DefMap { defDefault :: v, defMap :: M.Map k v }
  deriving (Show)

-- Instance
instance Functor (DefaultMap k) where
  fmap f dm =
    DefMap { defDefault = f (defDefault dm), defMap = f <$> defMap dm }

mapOnMap :: (M.Map k v -> M.Map kk v) -> DefaultMap k v -> DefaultMap kk v
mapOnMap f m = DefMap (defDefault m) $ f (defMap m)

-- Construction
empty :: v -> DefaultMap k v
empty def = DefMap def M.empty

singleton :: v -> k -> v -> DefaultMap k v
singleton def k v = DefMap def $ M.singleton k v

fromList :: Ord k => v -> [(k, v)] -> DefaultMap k v
fromList def xs = DefMap def $ M.fromList xs

-- Insertion
insert :: Ord k => k -> v -> DefaultMap k v -> DefaultMap k v
insert k x = mapOnMap (M.insert k x)

-- Deletion/Update
delete :: Ord k => k -> DefaultMap k v -> DefaultMap k v
delete k = mapOnMap (M.delete k)

-- Query
lookup :: (Ord k) => k -> DefaultMap k v -> v
lookup k m = fromMaybe (defDefault m) $ M.lookup k (defMap m)

(!) :: Ord k => DefaultMap k v -> k -> v
m ! k = lookup k m

member :: Ord k => k -> DefaultMap k v -> Bool
member k = M.member k . defMap

notMember :: Ord k => k -> DefaultMap k v -> Bool
notMember k = M.notMember k . defMap

null :: DefaultMap k v -> Bool
null m = size m == 0

size :: DefaultMap k v -> Int
size = M.size . defMap

-- Transformation
toList :: DefaultMap k v -> [(k, v)]
toList = M.toList . defMap

keys :: DefaultMap k v -> [k]
keys = fmap fst . toList
