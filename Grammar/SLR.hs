{-# LANGUAGE PackageImports, FlexibleInstances, MultiParamTypeClasses #-}
module SLR where
import Control.Applicative
import qualified Control.Arrow as A
import "monads-fd" Control.Monad.Reader
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S
import Data.Maybe

import Aux
import Item
import Table
import Token
import Untyped

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

----------------------------------

-- | Create SLR parsing tables from a starting rule of a grammar (augmented)
slr :: Token s => RId s -> (ActionTable s, GotoTable s, Int)
slr g =
    let initg      = gen g
        cs         = gItemSets initg
        as         = M.unions [runGen (actions i) initg | (i,_) <- cs]
        gs         = M.unions [runGen (gotos   i) initg | (i,_) <- cs]
    in (as, gs, gStartState initg)

-- | Create goto table
gotos :: Token s => Set (Item s) -> Gen Item s (GotoTable s)
gotos items = do
    Just i <- askItemSet items
    nt     <- asks gNonTerminals
    M.fromList <$> catMaybes <$> sequence
        [do j <- askItemSet (goto items a)
            return $ case j of
                Nothing -> Nothing
                Just x  -> Just ((i, ai), x)
          | a@(SRule (RId ai _)) <- nt]

-- | Create action table
actions :: Token s => Set (Item s) -> Gen Item s (ActionTable s)
actions items = do
    Just i <- askItemSet items
    start  <- asks gStartRule
    rs     <- asks gRules
    let actions' item@Item {itemRId = rid@(RId ri _)} = case nextSymbol item of
            Tok a@(STerm s) -> do
                j <- askItemSet $ goto items a
                case j of
                    Just x  -> return [(Tok s, Shift x)]
                    Nothing -> return []
            RightEnd
                | rid /= start -> do
                    let as = S.toList $ follow rid start rs
                    return [(a, Reduce ri (itProd item, length (getItProd item)))
                           | a <- as]
                | otherwise     -> return [(RightEnd, Accept)]
            _ -> return []
    M.fromList
        <$> concat
        <$> sequence
            [map (A.first ((,) i)) <$> actions' it | it <- S.toList items]

