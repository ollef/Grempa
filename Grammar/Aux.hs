module Aux where
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as S

setFromJust :: Ord a => Set (Maybe a) -> Set a
setFromJust = S.map fromJust . S.delete Nothing

-- | Traverse a recursive data structure without doing the same thing more
--   than once.
--   Takes a function returning (result, candidates), then the initial set
recTraverseG :: (Ord a, Ord b) => (Set a -> (Set b, Set a)) -> Set a -> Set b
recTraverseG = recTraverseG' S.empty
  where
    recTraverseG' done f x = if S.null cand'
                              then res
                              else res `S.union` recTraverseG' done' f cand'
      where (res, cand) = f x
            cand'       = cand S.\\ done'
            done'       = done `S.union` x

recTraverse :: Ord a => (Set a -> Set a) -> Set a -> Set a
recTraverse f = recTraverseG $ split . f
  where split x = (x, x)
