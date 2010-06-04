{-# LANGUAGE DoRec, PackageImports, NoMonomorphismRestriction, GADTs, DeriveDataTypeable #-}
module LR where
import Control.Applicative
import qualified Control.Arrow as A
import "monads-fd" Control.Monad.State
import "monads-fd" Control.Monad.Reader
import Data.Data
import Data.Dynamic
import Data.Function
import Data.Map(Map)
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S
import Data.Maybe
import Data.List

import Debug.Trace

import Aux
import qualified Typed as T
import Untyped

-- Get all rules from a grammar (recursively)
rules :: Ord s => RId s -> [RId s]
rules = S.toList . recTraverseG rules' . S.singleton
  where
    rules' rs     = (res `S.union` rs, res)
      where
        res = S.unions $ map aux (S.toList rs)
    aux (RId i r) = S.fromList [rid | p <- r, SRule rid <- p]

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
nextSymbol i@(Item rid _ pos)
    | pos < length prod = Just $ prod !! pos
    | otherwise               = Nothing
  where prod = getItProd i

-- | Get the items with the dot at the beginning from a rule
firstItems :: RId s -> Set (Item s)
firstItems rid@(RId _ prods) = S.fromList
                             $ map (\p -> Item rid p 0) [0..length prods - 1]

-- | Determine the state transitions in the parsing
goto :: Eq s => Set (Item s) -> Symbol s -> Set (Item s)
goto is x = closure $ setFromJust $ S.map (nextTest x) is
  where
    nextTest x i
      | nextSymbol i == Just x = Just i { itPos = itPos i + 1 }
      | otherwise            = Nothing

-- | The sets of items for a grammar
itemSets :: (Ord s, Show s) => RId s -> [RId s] -> Set (Set (Item s))
itemSets rid rids = recTraverseG itemSets' c1
  where
    c1            = S.singleton $ closure $ S.singleton $ Item rid 0 0
    symbols       = terminals rids ++ nonTerminals rids
    itemSets' c   = (c `S.union` gotos, gotos)
      where
        gotos   = S.fromList [goto i x | i <- S.toList c, x <- symbols]

----------------------------------

-- | Get all terminals (input symbols) from a list of rule IDs
terminals :: Ord s => [RId s] -> [Symbol s]
terminals = concatMap (\(RId _ rs) -> [STerm s | as <- rs, STerm s <- as])

-- | Get all non-terminals (variables) from a list of rule IDs
nonTerminals :: Ord s => [RId s] -> [Symbol s]
nonTerminals = map SRule

-- | Get the first symbols that a symbol eats, Nothing means epsilon
first :: Ord s => Symbol s -> Set (Maybe s)
first = first' S.empty

first' :: Ord s => Set (RId s) -> Symbol s -> Set (Maybe s)
first' done (STerm s)       = S.singleton (Just s)
first' done (SRule rid@(RId _ r)) = case rid `S.member` done of
    False -> S.unions $ map (firstProd' $ S.insert rid done) r
    True  -> S.empty

-- | Get the first symbols of a production
firstProd :: Ord s => Prod s -> Set (Maybe s)
firstProd = firstProd' S.empty

firstProd' :: Ord s => Set (RId s) -> Prod s -> Set (Maybe s)
firstProd' done []     = S.singleton Nothing
firstProd' done (x:[]) = first' done x
firstProd' done (x:xs) = case Nothing `S.member` fx of
    True  -> S.union fx' (firstProd' done xs)
    False -> fx'
  where
    fx  = first' done x
    fx' = S.delete Nothing fx

-- | Get all symbols that can follow a rule, Nothing means right end marker
follow :: Ord s => RId s -> RId s -> [RId s] -> Set (Maybe s)
follow = follow' S.empty

follow' :: Ord s => Set (RId s) -> RId s -> RId s -> [RId s] -> Set (Maybe s)
follow' done rid startrid rids = case rid `S.member` done of
    True  -> S.empty
    False -> S.unions $
        (if rid == startrid then S.singleton Nothing else S.empty) :
        [followProd prod a | a@(RId _ prods) <- rids, prod <- prods]
  where
    followProd []       a = S.empty
    followProd (b:beta) a
        | b == SRule rid = case Nothing `S.member` firstbeta of
            True  -> follow' (S.insert rid done)
                             a startrid rids `S.union` rest
            False -> rest
        | otherwise      = followProd beta a
      where
        firstbeta = firstProd beta
        rest      = S.delete Nothing firstbeta

-- | Data type used in the action table to determine the next
--   parsing action depending on the input and current state
data Action
    = Shift  Int
    | Reduce Int (Int, Int)
    | Accept
    | Error
  deriving Show

type ActionTable s = Map (Int, Maybe s)   Action
type GotoTable   s = Map (Int, Int) Int

type SLR s a = Reader (SLRState s) a
data SLRState s = SLRState
    {
      slrItemSets :: Map (Set (Item s))   Int
    }

slrLookup :: (Ord a, Show a) => a -> (SLRState s -> Map a Int) -> SLR s Int
slrLookup x f = do
    r <- M.lookup x <$> asks f
    case r of
        Just r -> return r
        Nothing -> error $ "slrLookup, Nothing" ++ show x

askItemSet :: (Ord s, Show s) => Set (Item s) -> SLR s (Maybe Int)
askItemSet x = M.lookup x <$> asks slrItemSets

-- | Create SLR parsing tables from a starting rule of a grammar (augmented)
slr :: (Typeable s, Ord s, Show s)
    => RId s -> (ActionTable s, GotoTable s,Int)
slr g =
    let rs = rules g
        c   = S.toList $ itemSets g rs
        cis = M.fromList $ zip c [0..]
        slrs = SLRState {slrItemSets = cis}
        as  = M.unions [runReader (actions i g rs) slrs | i <- c]
        gs  = M.unions [runReader (gotos   i   rs) slrs | i <- c]
        start = Item g 0 0
        startState = snd $ maybe (error "slr") id
                         $ find (\(c, _) -> start `S.member` c) (M.toList cis)
    in (as, gs, startState)

-- | Create goto table
gotos :: (Ord s, Show s) => Set (Item s) -> [RId s] -> SLR s (GotoTable s)
gotos items rules = do
    Just i <- askItemSet items
    M.fromList <$> catMaybes <$> sequence
        [do j <- askItemSet (goto items a)
            return $ case j of
                Nothing -> Nothing
                Just j  -> Just ((i, ai), j)
          | a@(SRule (RId ai _)) <- nonTerminals rules]


-- | Create action table
actions :: (Ord s, Show s)
        => Set (Item s) -> RId s -> [RId s] -> SLR s (ActionTable s)
actions items start rules = do
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
                Just j  -> return [(Just s, Shift j)]
                Nothing -> return []
        Nothing
            | rid /= start -> do
                let as = S.toList $ follow rid start rules
                return [(a, Reduce ri (itProd item, length (getItProd item)))
                       | a <- as]
            | otherwise     -> return [(Nothing, Accept)]
        _ -> return []

-- | Data type for reduction trees output by the driver
data ReductionTree s
    = RTReduce Int Int [ReductionTree s]
    | RTTerm (Maybe s)
  deriving Show

driver :: (Ord s, Show s)
       => (ActionTable s, GotoTable s, Int) -> [s] -> ReductionTree s
driver (action, goto, start) input =
    driver' [start] (map Just input ++ [Nothing]) []
  where
    driver' stack@(s:_) (a:rest) rt = --trace (show stack ++ "," ++ show (a:rest) ++ show action ++ show goto)
      case M.lookup (s, a) action of
        Just (Shift t) -> driver' (t : stack) rest (RTTerm a : rt)
        Just (Reduce rule (prod, len)) -> driver' (got : stack') (a : rest) rt'
          where
            stack'@(t:_) = drop len stack
            got          = case M.lookup (t, rule) goto of
                Just i  -> i
                Nothing -> error $ "goto " ++ show t ++ " " ++ show rule
            rt' = RTReduce rule prod (reverse $ take len rt) : drop len rt
        Just Accept -> head rt
        _      -> error $ "Wrong, wrong, absolutely briming over with wrongability! " ++ show (s, a)

rtToTyped :: Typeable s => (s' -> s) -> ProdFuns -> ReductionTree s' -> Dynamic
rtToTyped unc funs (RTTerm (Just s))     = toDyn (unc s)
rtToTyped unc funs (RTReduce ri pi tree) = trace (show (T.applDynFun fun l)) $ T.applDynFun fun l
  where
    l             = map (rtToTyped unc funs) tree
    fun           = fromJust $ M.lookup (ri, pi) funs

runSLRG :: (Data s, Typeable s', Typeable a, Ord s', Show s')
       => (s -> s') -> (s' -> s) -> T.Grammar s (T.RId s a) -> [s] -> T.Grammar s a
runSLRG c unc g inp = do
    g' <- T.augment g
    let (unt, funs) = unType c g'
        tables      = slr unt
        res         = driver tables (map c inp)
    return $ fromJust $ fromDynamic $ rtToTyped unc funs res

-- | Type for representing tokens only caring about the constructor
data CTok a where
    CTok :: {unCTok :: a} -> CTok a
  deriving (Show, Typeable)

instance Data a => Eq (CTok a) where
    CTok x == CTok y = ((==) `on` toConstr) x y

instance (Data a, Ord a) => Ord (CTok a) where
    CTok x `compare` CTok y = case ((==) `on` toConstr) x y of
        True  -> EQ
        False -> x `compare` y

runSLR = runSLRG id id
runSLRC = runSLRG CTok unCTok
