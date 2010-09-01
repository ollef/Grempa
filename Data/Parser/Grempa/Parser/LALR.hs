{-# LANGUAGE TupleSections, DoRec, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Parser.Grempa.Parser.LALR where

import Control.Applicative
import qualified Control.Arrow as A
import Control.Monad.Reader
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as S

import Data.Parser.Grempa.Aux.Aux
import Data.Parser.Grempa.Parser.Item
import Data.Parser.Grempa.Aux.MultiMap(MultiMap)
import qualified Data.Parser.Grempa.Aux.MultiMap as MM
import qualified Data.Parser.Grempa.Parser.SLR as SLR
import Data.Parser.Grempa.Parser.Table
import Data.Parser.Grempa.Grammar.Token
import Data.Parser.Grempa.Grammar.Untyped

data Item s =
     Item { itemRId  :: RId s
          , itemProd :: Int
          , itemPos  :: Int
          , itemLA   :: Tok s
          }
  deriving (Eq, Ord)

instance Show s => Show (Item s) where
  show (Item r pr po la) = "It(" ++ show r  ++
                             "," ++ show pr ++
                             "," ++ show po ++
                             "," ++ show la ++ ")\n"


instance Token s => It Item s where
    itRId         = itemRId
    itProd        = itemProd
    getItPos      = itemPos
    setItPos i p  = i {itemPos = p}
    closure       = closureLR1
    startItem rid = Item rid 0 0 EOF


-- | Determine what items may be valid productions from an item
closureLR1 :: Token s => Set (Item s) -> Set (Item s)
closureLR1 = recTraverseG closure'
  where
    closure' is = (is `S.union` res, res)
      where res = S.unions $ map closureI $ S.toList is
    closureI i = case nextSymbol i of
        Tok (SRule rid) -> S.unions [firstItems rid b | b <- firstA beta (itemLA i)]
          where beta = drop (getItPos i + 1) (getItProd i)
        _               -> S.empty
    firstA prod sym = let f = firstProd prod in
        if Epsilon `S.member` f
            then S.toList (S.insert sym $ unETokSet f)
            else map (Tok . unETok) $ S.toList f
    unETokSet = S.map (Tok . unETok) . S.delete Epsilon
    -- | Get the items with the dot at the beginning from a rule
    firstItems :: Token s => RId s -> Tok s -> Set (Item s)
    firstItems rid@(RId _ prods) a = S.fromList
                                   $ map (\p -> Item rid p 0 a)
                                   [0..length prods - 1]

data Lookahead s
    = Spont (Tok s)
    | PropFrom Int (SLR.Item s)
  deriving (Eq, Ord, Show)

-- Using Maybe where Nothing represents a symbol not in the grammar
type LookaheadTable s = MultiMap (Int, SLR.Item  (Maybe s))
                                 (Lookahead (Maybe s))

-- | Compute how the lookaheads propagate
lookaheads :: Token s
           => Int
           -> Set (SLR.Item (Maybe s))
           -> Set (SLR.Item (Maybe s))
           -> Symbol (Maybe s)
           -> Gen SLR.Item (Maybe s) (LookaheadTable s)
lookaheads istate i k x = do
    mjstate <- askItemSet (goto i x)
    case mjstate of
        Nothing -> return MM.empty
        Just jstate -> do
            startSt  <- asks gStartState
            startRId <- asks gStartRule
            let startIt = startItem startRId
            return $ MM.insert (startSt, startIt) (Spont EOF)
                   $ MM.unions
                   $ map (MM.fromList . lookaheadsI jstate)
                   $ S.toList k
  where
    lookaheadsI jstate a
        = [case itemLA b /= Tok Nothing of
               True  -> ((jstate, nextItPos $ fromLALR b), Spont $ itemLA b)
               False -> ((jstate, nextItPos $ fromLALR b), PropFrom istate a)
          | b <- S.toList js
          , nextSymbol b == Tok x]
      where js  = closure $ S.singleton $ fromSLR a (Tok Nothing)

fromSLR :: SLR.Item s -> Tok s -> Item s
fromSLR (SLR.Item r prod pos) = Item r prod pos

fromLALR :: Item s -> SLR.Item s
fromLALR (Item r prod pos _) = SLR.Item r prod pos

-- | Find the lookaheads of an SLR Item
findLookaheads :: Token s
               => LookaheadTable s
               -> Int -> SLR.Item (Maybe s)
               -> Done (Int, SLR.Item (Maybe s)) () (Set (Tok (Maybe s)))
findLookaheads latable istate i =
    ifNotDoneG (istate, i) (const S.empty) $ do
        let las = MM.lookup (istate, i) latable
        putDone (istate, i) ()
        S.unions <$> mapM go (S.toList las)
  where
    go (Spont s)        = return $ S.singleton s
    go (PropFrom st it) = findLookaheads latable st it

-- | Construct the LALR items from a set of SLR items
lalrItems :: Token s => Gen SLR.Item (Maybe s) [(Set (Item (Maybe s)), Int)]
lalrItems = do
    st  <- asks gStartRule
    iss <- asks gItemSets
    let kss  = map (A.first $ kernel st) iss
    syms <- asks gSymbols
    las <- zipWithM (\(i,n) (k,_) -> MM.unions <$> mapM (lookaheads n i k) syms) iss kss
    let tab = MM.unions las
    return
        [ let newi = [ evalDone $ toIts it <$> findLookaheads tab n it
                     | it <- S.toList ks]
          in (closure $ S.fromList $ concat newi, n)
        | (ks, n) <- kss]
  where
    toIts it   las = map (fromSLR it) $ remNothing las
    remNothing las = S.toList $ S.delete (Tok Nothing) las

slrGenToLalrGen :: Token s => GenData SLR.Item (Maybe s) -> GenData Item (Maybe s)
slrGenToLalrGen g = let newits = runGen lalrItems g
                    in g { gItemSets     = newits
                         , gItemSetIndex = M.fromList newits
                         }
-- | Create LALR parsing tables from a starting rule of a grammar (augmented)
lalr :: Token s => RId s -> (ActionTable s, GotoTable s, Int)
lalr g =
    let initSlr    = gen (Just <$> g)
        initg      = slrGenToLalrGen initSlr
        cs         = gItemSets initg
        as         =        [runGen (actions i) initg | i <- cs]
        gs         = concat [runGen (gotos   i) initg | i <- cs]
    in (as, gs, gStartState initg)

-- | Create goto table
gotos :: Token s
      => (Set (Item (Maybe s)), StateI)
      -> Gen Item (Maybe s) [((StateI, RuleI), StateI)]
gotos (items, i) = do
    nt     <- asks gNonTerminals
    map (A.first (i,)) <$> catMaybes <$> sequence
        [do j <- askItemSet $ goto items a
            return $ case j of
                Nothing -> Nothing
                Just x  -> Just (ai, x)
          | a@(SRule (RId ai _)) <- nt]

-- | Create action table
actions :: Token s
        => (Set (Item (Maybe s)), StateI)
        -> Gen Item (Maybe s) (StateI, [(Tok s, Action s)], Action s))
actions (items, i) = do
    start  <- asks gStartRule
    let actions' item@Item {itemRId = rid@(RId ri _)} = case nextSymbol item of
            Tok a@(STerm (Just s)) -> do
                j <- askItemSet $ goto items a
                case j of
                    Just x  -> return [(Tok s, Shift x)]
                    Nothing -> return []
            EOF
                | rid /= start ->
                    return
                        [ ( fromJust <$> itemLA item
                          , Reduce ri (itProd item)
                                      (length $ getItProd item) [])]
                | itemLA item == EOF -> return [(EOF, Accept)]
            _ -> return []
    tab <- concat <$> sequence
            [actions' it | it <- S.toList items]
    return (i, (mapShifts tab, def (mapShifts tab)))
  where
    def tab = if M.null (reds tab)
        then Error $ M.keys $ shifts tab
        else head (M.elems $ reds tab)
    mapShifts tab = M.map (addShifts $ M.keys $ shifts tab) tab
      where addShifts ss (Reduce r pr p _) = Reduce r pr p ss
            addShifts _  x                 = x
    shifts = M.filter (not . isReduce)
    reds   = M.filter isReduce
