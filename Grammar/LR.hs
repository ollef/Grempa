{-# LANGUAGE PackageImports, TemplateHaskell #-}
module LR where
import Control.Applicative
import qualified Control.Arrow as A
import "monads-fd" Control.Monad.Reader
--import Data.Data
import Data.Dynamic
import Data.Map(Map)
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S
import Data.Maybe
import Data.List

import Debug.Trace

import Aux
import qualified Typed as T
import Table
import Token
import Untyped

-- Get all rules from a grammar (recursively)
rules :: Token s => RId s -> [RId s]
rules = S.toList . recTraverseG rules' . S.singleton
  where
    rules' rs     = (res `S.union` rs, res)
      where
        res = S.unions $ map aux (S.toList rs)
    aux (RId _ r) = S.fromList [rid | p <- r, SRule rid <- p]

-- | Determine what items may be valid productions from an item
closure :: Set (Item s) -> Set (Item s)
closure = recTraverseG closure'
  where
    closure' is = (is `S.union` res, res)
      where res = S.unions $ map closureI (S.toList is)
    closureI i = case nextSymbol i of
        Just (SRule rid) -> firstItems rid
        _                -> S.empty

-- | Return the atom to the right of the "dot" in the item
--   returns Nothing if the dot is rightmost in the item
nextSymbol :: Item s -> Maybe (Symbol s)
nextSymbol i
    | pos < length prod = Just $ prod !! pos
    | otherwise               = Nothing
  where prod = getItProd i
        pos  = itPos i

-- | Get the items with the dot at the beginning from a rule
firstItems :: RId s -> Set (Item s)
firstItems rid@(RId _ prods) = S.fromList
                             $ map (\p -> Item rid p 0) [0..length prods - 1]

-- | Determine the state transitions in the parsing
goto :: Token s => Set (Item s) -> Symbol s -> Set (Item s)
goto is s = closure $ setFromJust $ S.map (nextTest s) is
  where
    nextTest x i
      | nextSymbol i == Just x = Just i { itPos = itPos i + 1 }
      | otherwise            = Nothing

-- | The sets of items for a grammar
itemSets :: Token s => RId s -> [RId s] -> Set (Set (Item s))
itemSets rid rids = recTraverseG itemSets' c1
  where
    c1            = S.singleton $ closure $ S.singleton $ Item rid 0 0
    symbols       = terminals rids ++ nonTerminals rids
    itemSets' c   = (c `S.union` gs, gs)
      where gs    = S.fromList [goto i x | i <- S.toList c, x <- symbols]

----------------------------------

-- | Get all terminals (input symbols) from a list of rule IDs
terminals :: Token s => [RId s] -> [Symbol s]
terminals = concatMap (\(RId _ rs) -> [STerm s | as <- rs, STerm s <- as])

-- | Get all non-terminals (variables) from a list of rule IDs
nonTerminals :: Token s => [RId s] -> [Symbol s]
nonTerminals = map SRule

-- | Get the first symbols that a symbol eats, Nothing means epsilon
first :: Token s => Symbol s -> Set (Maybe s)
first = first' S.empty

first' :: Token s => Set (RId s) -> Symbol s -> Set (Maybe s)
first' _    (STerm s)              = S.singleton (Just s)
first' done (SRule rid@(RId _ r)) = case rid `S.member` done of
    False -> S.unions $ map (firstProd' $ S.insert rid done) r
    True  -> S.empty

-- | Get the first symbols of a production
firstProd :: Token s => Prod s -> Set (Maybe s)
firstProd = firstProd' S.empty

firstProd' :: Token s => Set (RId s) -> Prod s -> Set (Maybe s)
firstProd' _    []     = S.singleton Nothing
firstProd' done (x:[]) = first' done x
firstProd' done (x:xs) = case Nothing `S.member` fx of
    True  -> S.union fx' (firstProd' done xs)
    False -> fx'
  where
    fx  = first' done x
    fx' = S.delete Nothing fx

-- | Get all symbols that can follow a rule, Nothing means right end marker
follow :: Token s => RId s -> RId s -> [RId s] -> Set (Maybe s)
follow = follow' S.empty

follow' :: Token s => Set (RId s) -> RId s -> RId s -> [RId s] -> Set (Maybe s)
follow' done rid startrid rids = case rid `S.member` done of
    True  -> S.empty
    False -> S.unions $
        (if rid == startrid then S.singleton Nothing else S.empty) :
        [followProd prod a | a@(RId _ prods) <- rids, prod <- prods]
  where
    followProd []       _ = S.empty
    followProd (b:beta) a
        | b == SRule rid = case Nothing `S.member` firstbeta of
            True  -> follow' (S.insert rid done)
                             a startrid rids `S.union` rest
            False -> rest
        | otherwise      = followProd beta a
      where
        firstbeta = firstProd beta
        rest      = S.delete Nothing firstbeta

type SLR s a = Reader (SLRState s) a
data SLRState s = SLRState
    {
      slrItemSets :: Map (Set (Item s))   Int
    }

slrLookup :: (Ord a, Show a) => a -> (SLRState s -> Map a Int) -> SLR s Int
slrLookup x f = do
    res <- M.lookup x <$> asks f
    case res of
        Just r  -> return r
        Nothing -> error $ "slrLookup, Nothing" ++ show x

askItemSet :: (Ord s, Show s) => Set (Item s) -> SLR s (Maybe Int)
askItemSet x = M.lookup x <$> asks slrItemSets

-- | Create SLR parsing tables from a starting rule of a grammar (augmented)
slr :: Token s => RId s -> (ActionTable s, GotoTable s,Int)
slr g =
    let rs         = rules g
        cs         = S.toList $ itemSets g rs
        cis        = M.fromList $ zip cs [0..]
        slrs       = SLRState {slrItemSets = cis}
        as         = M.unions [runReader (actions i g rs) slrs | i <- cs]
        gs         = M.unions [runReader (gotos   i   rs) slrs | i <- cs]
        start      = Item g 0 0
        startState = snd $ maybe (error "slr: maybe") id
                         $ find (\(c, _) -> start `S.member` c) (M.toList cis)
    in (as, gs, startState)

-- | Create goto table
gotos :: Token s => Set (Item s) -> [RId s] -> SLR s (GotoTable s)
gotos items rs = do
    Just i <- askItemSet items
    M.fromList <$> catMaybes <$> sequence
        [do j <- askItemSet (goto items a)
            return $ case j of
                Nothing -> Nothing
                Just x  -> Just ((i, ai), x)
          | a@(SRule (RId ai _)) <- nonTerminals rs]


-- | Create action table
actions :: Token s => Set (Item s) -> RId s -> [RId s] -> SLR s (ActionTable s)
actions items start rs = do
    Just i <- askItemSet items
    M.fromList
        <$> concat
        <$> sequence
            [map (A.first ((,) i)) <$> actions' it | it <- S.toList items]
  where
    --actions' :: Item s -> SLR s [(Maybe s, Action)]
    actions' item@Item {itRId = rid@(RId ri _)} = case nextSymbol item of
        Just a@(STerm s) -> do
            j <- askItemSet $ goto items a
            case j of
                Just x  -> return [(Just s, Shift x)]
                Nothing -> return []
        Nothing
            | rid /= start -> do
                let as = S.toList $ follow rid start rs
                return [(a, Reduce ri (itProd item, length (getItProd item)))
                       | a <- as]
            | otherwise     -> return [(Nothing, Accept)]
        _ -> return []

-- | Data type for reduction trees output by the driver
data ReductionTree s
    = RTReduce Int Int [ReductionTree s]
    | RTTerm (Maybe s)
  deriving Show

driver :: Token s => (ActionFun s, GotoFun s, Int) -> [s] -> ReductionTree s
driver (actionf, gotof, start) input =
    driver' [start] (map Just input ++ [Nothing]) []
  where
    driver' stack@(s:_) (a:rest) rt = --trace (show stack ++ "," ++ show (a:rest) ++ show action ++ show goto)
      case actionf (s, a) of
        Shift t -> driver' (t : stack) rest (RTTerm a : rt)
        Reduce rule (prod, len) -> driver' (got : stack') (a : rest) rt'
          where
            stack'@(t:_) = drop len stack
            got          = gotof (t, rule)
            rt' = RTReduce rule prod (reverse $ take len rt) : drop len rt
        Accept -> head rt
    driver' _ _ _ = error "driver'"

rtToTyped :: Token s => (s' -> s) -> ProdFuns -> ReductionTree s' -> Dynamic
rtToTyped unc _    (RTTerm (Just s))     = toDyn (unc s)
rtToTyped unc funs (RTReduce r p tree) = T.applDynFun fun l
  where
    l             = map (rtToTyped unc funs) tree
    fun           = fromJust $ M.lookup (r, p) funs
rtToTyped _ _ _ = error "rtToTyped"

runSLRG :: (Token s', Token s, Typeable a) => (s -> s')
        -> T.GRId s a -> [s] -> T.Grammar s (ReductionTree s', ProdFuns)
runSLRG c g inp = do
    g' <- T.augment g
    let (unt, funs) = unType c g'
        (at,gt,st)  = slr unt
        res         = driver (toFun actionError at, toFun gotoError gt, st) $ map c inp
    return (res, funs)

runSLRGRes :: (Token s, Token s', Typeable a)
       => (s -> s') -> (s' -> s) -> T.GRId s a -> [s] -> T.Grammar s a
runSLRGRes c unc g inp = do
    (res, funs) <- runSLRG c g inp
    return $ fromJust $ fromDynamic $ rtToTyped unc funs res

runSLR  :: (Token s, Typeable a) => T.GRId s a -> [s] -> T.Grammar s a
runSLR  = runSLRGRes id id

runSLRC :: (Token s, Typeable a) => T.GRId s a -> [s] -> T.Grammar s a
runSLRC = runSLRGRes CTok unCTok

actionError, gotoError :: String
actionError = "ActionTable"
gotoError   = "GotoTable"

