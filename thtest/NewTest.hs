module NewTest where

import Data.Set(Set)
import qualified Data.Set as S

import Aux
import NewTestUntyped

rules :: Ord s => RId s -> [RId s]
rules = S.toList . recTraverseG rules' . S.singleton
  where
    rules' rs     = (res `S.union` rs, res)
      where
        res = S.unions $ map aux (S.toList rs)
    aux (RId i r) = S.fromList [rid | p <- r, SRule rid <- p]

