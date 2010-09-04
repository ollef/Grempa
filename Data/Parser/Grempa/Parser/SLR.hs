{-# LANGUAGE TupleSections, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Parser.Grempa.Parser.SLR
    ( Item(..)
    , slr
    ) where
import Control.Applicative
import qualified Control.Arrow as A
import Control.Monad.Reader
import Data.Set(Set)
import qualified Data.Set as S
import Data.Maybe

import Data.Parser.Grempa.Aux.Aux
import Data.Parser.Grempa.Parser.Item
import Data.Parser.Grempa.Parser.Table
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

----------------------------------

-- | Create SLR parsing tables from a starting rule of a grammar (augmented)
slr :: Token s => RId s -> (ActionTable s, GotoTable s, Int)
slr g =
    let initg      = gen g
        cs         = gItemSets initg
        as         =        [runGen (actions i) initg | i <- cs]
        gs         = concat [runGen (gotos   i) initg | i <- cs]
    in (as, gs, gStartState initg)

-- | Create goto table
gotos :: Token s
      => (Set (Item s), StateI)
      -> Gen Item s [((StateI, RuleI), StateI)]
gotos (items, i) = do
    nt     <- asks gNonTerminals
    map (A.first (i,)) <$> catMaybes <$> sequence
        [do j <- askItemSet (goto items a)
            return $ case j of
                Nothing -> Nothing
                Just x  -> Just (ai, x)
          | a@(SRule (RId ai _)) <- nt]

-- | Create action table
actions :: Token s
        => (Set (Item s), StateI)
        -> Gen Item s (StateI, ([(Tok s, Action s)], Action s))
actions (items, i) = do
    start  <- asks gStartRule
    rs     <- asks gRules
    let actions' item@Item {itemRId = rid@(RId ri _)} = case nextSymbol item of
            Tok a@(STerm s) -> do
                j <- askItemSet $ goto items a
                case j of
                    Just x  -> return [(Tok s, Shift x)]
                    Nothing -> return []
            EOF
                | rid /= start -> do
                    let as = S.toList $ follow rid start rs
                    return [(a, Reduce ri (itProd item) (length $ getItProd item) [])
                           | a <- as]
                | otherwise     -> return [(EOF, Accept)]
            _ -> return []
    tab <- concat <$> sequence
        [actions' it | it <- S.toList items]
    return (i, (mapShifts tab, def (mapShifts tab)))
  where
    def tab = if null (reds tab)
        then Error $ keys $ shifts tab
        else head  $ elems $ reds tab
    mapShifts tab = map (A.second $ addShifts $ keys $ shifts tab) tab
      where addShifts ss (Reduce r pr p _) = Reduce r pr p ss
            addShifts _  x                 = x
    reds   = filter (isReduce . snd)
    shifts = filter (not . isReduce . snd)
    keys   = map fst
    elems  = map snd
