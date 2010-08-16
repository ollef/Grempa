-- | A Map mapping multiple values to a key.
module MultiMap
  ( MultiMap
  , lookup
  , insert
  , inserts
  , union
  , unions
  , fromList
  , M.empty
  ) where

import qualified Data.Map as M
import Data.Map(Map)
import Prelude(flip, foldl, Ord, Maybe(..), uncurry, ($))
import qualified Data.Set as S
import Data.Set(Set)

type MultiMap k a = Map k (Set a)

lookup :: Ord k => k -> MultiMap k a -> Set a
lookup k m = case M.lookup k m of
    Nothing -> S.empty
    Just s  -> s

insert :: (Ord a, Ord k) => k -> a -> MultiMap k a -> MultiMap k a
insert k v m = M.insert k (S.insert v (lookup k m)) m

inserts :: (Ord a, Ord k) => k -> Set a -> MultiMap k a -> MultiMap k a
inserts k v m = M.insert k (S.union v (lookup k m)) m

union :: (Ord a, Ord k) => MultiMap k a -> MultiMap k a -> MultiMap k a
union m1 m2 = foldl (flip $ uncurry inserts) m1 $ M.toList m2

unions :: (Ord a, Ord k) => [MultiMap k a] -> MultiMap k a
unions = foldl union M.empty

fromList :: (Ord a, Ord k) => [(k, a)] -> MultiMap k a
fromList xs = foldl (flip $ uncurry insert) M.empty xs
