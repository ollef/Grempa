{-# LANGUAGE DoRec, FlexibleInstances, MultiParamTypeClasses, PackageImports #-}
module LALR where

import Control.Applicative
import qualified Control.Arrow as A
import "monads-fd" Control.Monad.Reader
import "monads-fd" Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as S

import Debug.Trace

import Aux
import Item
import MultiMap(MultiMap)
import qualified MultiMap as MM
import qualified SLR
import Table
import Token
import Untyped

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
                             "," ++ show la ++ ")"


instance Token s => It Item s where
    itRId         = itemRId
    itProd        = itemProd
    getItPos      = itemPos
    setItPos i p  = i {itemPos = p}
    closure       = closureLR1
    startItem rid = Item rid 0 0 RightEnd


-- | Determine what items may be valid productions from an item
closureLR1 :: Token s => Set (Item s) -> Set (Item s)
closureLR1 = recTraverseG closure'
  where
    closure' is = (is `S.union` res, res)
      where res = S.unions $ map closureI $ S.toList is
    closureI i = case nextSymbol i of
        Tok (SRule rid) ->
            S.unions [firstItems rid b | b <- S.toList $ firsts i]
        _               -> S.empty
      where
        firsts it = unETokSet $ firstProd $ itTail it
        itTail it = drop (getItPos it + 1) (getItProd it)
                  ++ case itemLA i of
                         Tok a   -> [STerm a]
                         _       -> []
    -- | Get the items with the dot at the beginning from a rule
    firstItems :: Token s => RId s -> Tok s -> Set (Item s)
    firstItems rid@(RId _ prods) a = S.fromList
                                   $ map (\p -> Item rid p 0 a)
                                   [0..length prods - 1]
    unETokSet :: Token s => Set (ETok s) -> Set (Tok s)
    unETokSet = S.map (Tok . unETok) . S.delete Epsilon

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
lookaheads istate i k x = 
  trace ("LOOKAHEADS: " ++ show (i, x) ++ show (goto i x)) $ do
    mjstate <- askItemSet (goto i x)
    case mjstate of
        Nothing -> return MM.empty
        Just jstate -> do
            startSt  <- asks gStartState
            startRId <- asks gStartRule
            let startIt = startItem startRId
            return $ MM.insert (startSt, startIt) (Spont RightEnd)
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
fromSLR (SLR.Item r prod pos) la = Item r prod pos la

fromLALR :: Item s -> SLR.Item s
fromLALR (Item r prod pos _) = SLR.Item r prod pos

tracer s x = trace (s ++ show x) x

-- | Find the lookaheads of an SLR Item
findLookaheads :: Token s
               => LookaheadTable s
               -> Int -> SLR.Item (Maybe s)
               -> State (Map (Int, SLR.Item (Maybe s))
                             (Set (Tok (Maybe s))))
                        (Set (Tok (Maybe s)))
findLookaheads latable istate i = trace (show (istate, i)) $ do
    done <- gets $ M.lookup (istate, i)
    case done of
        Just toks -> return toks
        Nothing   -> do
            let las = tracer "las: " $ MM.lookup (istate, i) latable
            rec
              modify $ M.insert (istate, i) res
              res <- S.unions <$> mapM go (S.toList $ S.delete (PropFrom istate i) las)
            return $ tracer "res: " res
  where
    go (Spont s)        = return $ S.singleton s
    go (PropFrom st it) = findLookaheads latable st it

-- | Construct the LALR items from a set of SLR items
lalrItems :: Token s => Gen SLR.Item (Maybe s) [(Set (Item (Maybe s)), Int)]
lalrItems = do
    st  <- asks gStartRule
    iss <- asks gItemSets
    let kss  = tracer "KERNELS: " $ map (A.first $ kernel st) $ tracer "ITEMS: " iss
    syms <- asks gSymbols
    las <- zipWithM (\(i,n) (k,_) -> MM.unions <$> mapM (lookaheads n i k) syms) iss kss
    --trace ("XXX" ++ show las ++ "XXX") $ return ()
    let tab = tracer "TAB: " $ MM.unions las
    return $ tracer "lalrItems: " $ flip evalState M.empty $ do
        sequence [ do
            newi <- sequence [ toIts it <$> findLookaheads tab n it
                             | it <- S.toList ks]
            return $ (closure $ S.fromList $ concat newi, n)
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
        init       = slrGenToLalrGen initSlr
        cs         = gItemSets init
        as         = M.unions [runGen (actions i) init | (i,_) <- cs]
        gs         = M.unions [runGen (gotos   i) init | (i,_) <- cs]
    in tracer "LALR: " $ (as, gs, gStartState init)

-- | Create goto table
gotos :: Token s
      => Set (Item (Maybe s)) -> Gen Item (Maybe s) (GotoTable s)
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
actions :: Token s
        => Set (Item (Maybe s)) -> Gen Item (Maybe s) (ActionTable s)
actions items = do
    Just i <- askItemSet items
    start  <- asks gStartRule
    rs     <- asks gRules
    let actions' item@Item {itemRId = rid@(RId ri _)} = case nextSymbol item of
            Tok a@(STerm (Just s)) -> do
                j <- askItemSet $ goto items a
                case j of
                    Just x  -> return [(Tok s, Shift x)]
                    Nothing -> return []
            RightEnd
                | rid /= start ->
                    return
                        [ ( fromJust <$> itemLA item
                          , Reduce ri ( itProd item
                                      , length $ getItProd item))]
                | itemLA item == RightEnd -> return [(RightEnd, Accept)]
            _ -> return []
    M.fromList
        <$> concat
        <$> sequence
            [map (A.first ((,) i)) <$> actions' it | it <- S.toList items]
