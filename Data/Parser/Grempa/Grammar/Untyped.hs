{-# LANGUAGE GADTs, DoRec #-}
module Data.Parser.Grempa.Grammar.Untyped
    ( Rule, Prod, Symbol(..), RId(..)
    , unType
    , rules, terminals, nonTerminals
    , firstProd, follow
    )where

import qualified Control.Arrow as A
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as S

import Data.Parser.Grempa.Aux.Aux
import qualified Data.Parser.Grempa.Aux.MultiMap as MM
import Data.Parser.Grempa.Aux.MultiMap(MultiMap)
import Data.Parser.Grempa.Parser.Table
import Data.Parser.Grempa.Grammar.Token
import qualified Data.Parser.Grempa.Grammar.Typed as T

-- | The recursive data types for untyped grammars
type Rule s = [Prod s]
type Prod s = [Symbol s]

data Symbol s
    = STerm s
    | SRule (RId s)
  deriving (Eq, Ord, Show)

data RId s = RId {rId :: RuleI, rIdRule :: Rule s}

instance Show (RId s) where
    show (RId i _) = show i
instance Eq (RId s) where
    RId i _ == RId j _ = i == j
instance Ord (RId s) where
    RId i _ `compare` RId j _ = i `compare` j

type UnTypeState s' = State (Map Int (RId s'), ProdFunTable)
-- | Returns an untyped tree representation of a typed grammar
--   together with a mapping from rule and production number to
--   a dynamic containing the construction function of the typed
--   production
unType :: (s -> s') -> T.RId s a -> (RId s', ProdFunTable)
unType cs = A.second snd . flip runState (M.empty, []) . unTypeR cs
  where
    unTypeR :: (s -> s') -> T.RId s a -> UnTypeState s' (RId s')
    unTypeR c (T.RId i r) = do
        (rids, funs) <- get
        case M.lookup i rids of
            Just x  -> return x
            Nothing -> do
                let newfuns = zip (zip (repeat i) [0..])
                                  (map T.getFun r)
                rec
                  put (M.insert i res rids, funs ++ newfuns)
                  res <- RId i <$> mapM (unTypeP c) r
                return res
    unTypeP :: (s -> s') -> T.Prod s a -> UnTypeState s' (Prod s')
    unTypeP c p = case p of
        T.PSeq  ps s -> liftM2 (++) (unTypeP c ps) ((:[]) <$> unTypeS c s)
        T.PSeqN ps s -> liftM2 (++) (unTypeP c ps) ((:[]) <$> unTypeS c s)
        T.PFun _    -> return []
    unTypeS :: (s -> s') -> T.Symbol s a -> UnTypeState s' (Symbol s')
    unTypeS c s = case s of
        T.STerm t -> return $ STerm (c t)
        T.SRule r -> SRule <$> unTypeR c r


instance Functor RId where
    fmap = flip evalState M.empty `dot` fmapR
      where
        fmapS :: (a -> b) -> Symbol a -> Done (RId a) (RId b) (Symbol b)
        fmapS f (STerm s) = return $ STerm $ f s
        fmapS f (SRule r) = do
            done <- getDone r
            case done of
              Just r' -> return $ SRule r'
              Nothing -> do
                  rec
                    putDone r res
                    res <- fmapR f r
                  return $ SRule res
        fmapR :: (a -> b) -> RId a -> DoneA (RId a) (RId b)
        fmapR f (RId n r) = RId n <$> mapM (mapM (fmapS f)) r

-------------------------------------------------------------------------------
-- | Get all rules from a grammar by following a rule's non-terminals recursively
rules :: Token s => RId s -> [RId s]
rules = S.toList . recTraverseG rules' . S.singleton
  where
    rules' rs     = (res `S.union` rs, res)
      where
        res = S.unions $ map aux (S.toList rs)
    aux (RId _ r) = S.fromList [rid | p <- r, SRule rid <- p]

-- | Get all terminals (input symbols) from a list of rule IDs
terminals :: Token s => [RId s] -> [Symbol s]
terminals = concatMap (\(RId _ rs) -> [STerm s | as <- rs, STerm s <- as])

-- | Get all non-terminals (variables) from a list of rule IDs
nonTerminals :: Token s => [RId s] -> [Symbol s]
nonTerminals = map SRule

-- | Datatype used in computing the first set
data RecETok s
    = RETok {unRETok :: ETok s}
    | IfEpsilon (RId s) (Prod s)
    | RRule (RId s)
  deriving (Eq, Ord)

type First s a = State (MultiMap (RId s) (RecETok s)) a

-- | Get the first tokens that a symbol eats
-- first :: Token s => Symbol s -> Set (ETok s)
-- first s = firstProd [s]

firstProd :: Token s => Prod s -> Set (ETok s)
firstProd = flip evalState MM.empty . firstProd'

firstProd' :: Token s => Prod s -> First s (Set (ETok s))
firstProd' as = do
    go (RId (-1) undefined) as
    fixf
    results <$> gets (MM.lookup (RId (-1) undefined))
  where
    go :: Token s => RId s -> Prod s -> First s ()
    go rid []          = modify $ MM.insert rid (RETok Epsilon)
    go rid (STerm s:_) = modify $ MM.insert rid (RETok (ETok s))
    go rid (SRule rid'@(RId _ ps):xs) = do
        modify $ MM.insert rid (RRule rid')
        ss <- gets $ MM.lookup rid'
        when (S.null ss) $ mapM_ (go rid') ps
        modify $ MM.insert rid (IfEpsilon rid' xs)

    f :: Token s => RId s -> RecETok s -> First s ()
    f rid rt@(IfEpsilon rid' xs) = do
        ss  <- gets (MM.lookup rid')
        when (S.member (RETok Epsilon) ss) $ do
            modify $ MM.delete rid rt
            go rid xs
    f rid rt@(RRule rid') = do
        ss <- gets (MM.lookup rid')
        when (clean ss) $ do
            modify $ MM.delete rid rt
        modify $ MM.inserts rid (S.filter isRETok ss)
    f _ _ = return ()

    clean :: Set (RecETok s) -> Bool
    clean = S.fold ((&&) . isRETok) True

    isRETok :: RecETok s -> Bool
    isRETok (RETok _) = True
    isRETok _         = False

    fs :: Token s => First s ()
    fs = do
        ss <- get
        sequence_ [f rid rt | (rid, rts) <- MM.toList ss, rt <- S.toList rts]

    results :: Token s => Set (RecETok s) -> Set (ETok s)
    results = S.map unRETok . S.filter isRETok

    fixf :: Token s => First s ()
    fixf = do
        state <- get
        fs
        state' <- get
        when (state /= state') fixf

-- | Get all symbols that can follow a rule,
--   also given the start rule and a list of all rules
follow :: Token s => RId s -> RId s -> [RId s] -> Set (Tok s)
follow rid = evalDone `dot` follow' rid

follow' :: Token s => RId s -> RId s -> [RId s] -> Done (RId s) () (Set (Tok s))
follow' rid startrid rids = ifNotDoneG rid (const S.empty) $ do
    putDone rid ()
    (if rid == startrid then S.insert EOF else id)
        <$> S.unions
        <$> sequence [followProd prod a
                         | a@(RId _ prods) <- rids
                         , prod <- prods]
  where
    followProd []       _ = return S.empty
    followProd (b:beta) a
        | b == SRule rid =  S.union rest
                        <$> liftM2 S.union (followProd beta a)
                                           (if Epsilon `S.member` firstbeta
                                               then follow' a startrid rids
                                               else return S.empty)
        | otherwise      = followProd beta a
      where
        firstbeta = firstProd beta
        rest      = S.map (Tok . unETok) $ S.delete Epsilon firstbeta
