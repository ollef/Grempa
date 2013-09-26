{-# LANGUAGE TupleSections, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Parser.Grempa.Parser.SLR
    ( Item(..)
    ) where
import Data.Set(Set)
import qualified Data.Set as S

import Data.Parser.Grempa.Auxiliary.Auxiliary
import Data.Parser.Grempa.Parser.Item
import Data.Parser.Grempa.Grammar.Token
import Data.Parser.Grempa.Grammar.Untyped

data Item s =
     Item { itemRId  :: RId s
          , itemProd :: Int
          , itemPos  :: Int
          }
  deriving (Eq, Ord)

instance Show (Item s) where
  show (Item r pr po) = "It(" ++ show r  ++
                          "," ++ show pr ++
                          "," ++ show po ++ ")"

instance Token s => It Item s where
    itRId         = itemRId
    itProd        = itemProd
    getItPos      = itemPos
    setItPos i p  = i {itemPos = p}
    closure       = closureLR0
    startItem rid = Item rid 0 0

-- | Determine what items may be valid productions from an item
closureLR0 :: Token s => Set (Item s) -> Set (Item s)
closureLR0 = recTraverseG closure'
  where
    closure' is = (is `S.union` res, res)
      where res = S.unions $ map closureI (S.toList is)
    closureI i = case nextSymbol i of
        Tok (SRule rid) -> firstItems rid
        _               -> S.empty
    -- | Get the items with the dot at the beginning from a rule
    firstItems :: RId s -> Set (Item s)
    firstItems rid@(RId _ prods) = S.fromList
                                 $ map (\p -> Item rid p 0) [0..length prods - 1]
