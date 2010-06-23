{-# LANGUAGE PackageImports #-}
module LALR where
import Control.Applicative
import qualified Control.Arrow as A
import "monads-fd" Control.Monad.Reader
import Data.Map(Map)
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S
import Data.Maybe
import Data.List

--import Debug.Trace

import Aux
import qualified SLR
import Table
import Token
import Untyped

data Item s =
     Item { itRId  :: RId s
          , itProd :: Int
          , itPos  :: Int
          , itTok  :: Tok s
          }
  deriving (Eq, Ord, Show)

getItProd :: Item s -> Prod s
getItProd i = rIdRule (itRId i) !! itProd i

unETokSet :: Token s => Set (ETok s) -> Set (Tok s)
unETokSet = S.map (Tok . unETok) . S.delete Epsilon

-- | Determine what items may be valid productions from an item
closure :: Token s => Set (Item s) -> Set (Item s)
closure = recTraverseG closure'
  where
    closure' is = (is `S.union` res, res)
      where res = S.unions $ map closureI $ S.toList is
    closureI i = case nextSymbol i of
        Tok (SRule rid) ->
            S.unions [firstItems rid b | b <- S.toList $ firsts i]
        _               -> S.empty
      where
        firsts i = unETokSet $ SLR.firstProd $ itTail i
        itTail i = drop (itPos i + 1) (getItProd i) ++ case itTok i of
            Tok a   -> [STerm a]
            _       -> []
    -- | Get the items with the dot at the beginning from a rule
    firstItems :: Token s => RId s -> Tok s -> Set (Item s)
    firstItems rid@(RId _ prods) a = S.fromList
                                   $ map (\p -> Item rid p 0 a)
                                   [0..length prods - 1]

-- | The sets of items for a grammar
itemSets :: Token s => RId s -> [RId s] -> Set (Set (Item s))
itemSets rid rids = recTraverseG itemSets' c1
  where
    c1            = S.singleton $ closure $ S.singleton $ Item rid 0 0 RightEnd
    symbols       = SLR.terminals rids ++ SLR.nonTerminals rids
    itemSets' c   = (c `S.union` gs, gs)
      where gs    = S.fromList [goto i x | i <- S.toList c, x <- symbols]


-- | Given the start rule and an item,
--   determine if it is a kernel item
isKernel :: RId s -> SLR.Item s -> Bool
isKernel st it = SLR.itRId it == st || SLR.itPos it > 0

kernel :: RId s -> Set (SLR.Item s) -> Set (SLR.Item s)
kernel st = S.filter $ isKernel st
{-
data LookaheadGen s
    = Spont (Lookahead s)
    | Prop  (Item s)

lookaheads :: Set (SLR.Item s) -> Symbol s -> ..
lookaheads i x = mapM ? 
-}
