{-# LANGUAGE GADTs, DoRec, PackageImports #-}
module Grammar.Untyped where

import Control.Arrow
import Control.Applicative
import "monads-fd" Control.Monad.State
import qualified Data.Map as M
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as S

import Aux
import Parser.Table
import Grammar.Token
import qualified Grammar.Typed as T

import Debug.Trace

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

-- | Returns an untyped tree representation of a typed grammar
--   together with a mapping from rule and production number to
--   a dynamic containing the construction function of the typed
--   production
type UnTypeState s' = State (Map Int (RId s'), ProdFunTable)
unType :: (s -> s') -> T.RId s a -> (RId s', ProdFunTable)
unType cs = second snd . flip runState (M.empty, []) . unTypeR cs
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
                  res <- RId i <$> mapM ((reverse <$>) . unTypeP c) r
                return res
    unTypeP :: (s -> s') -> T.Prod s a -> UnTypeState s' (Prod s')
    unTypeP c p = case p of
        T.PSeq  s ps -> liftM2 (:) (unTypeS c s) (unTypeP c ps)
        T.PSeqN s ps -> liftM2 (:) (unTypeS c s) (unTypeP c ps)
        T.PEnd _    -> return []
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

-- | Get the first tokens that a symbol eats
first :: Token s => Symbol s -> Set (ETok s)
first = evalDone . first'

first' :: Token s => Symbol s -> Done (RId s) () (Set (ETok s))
first' (STerm s)             = return $ S.singleton (ETok s)
first' (SRule rid@(RId _ r)) = ifNotDoneG rid (const S.empty) $ do
    putDone rid () -- res
    S.unions <$> mapM firstProd' r

-- | Get the first tokens of a production
firstProd :: Token s => Prod s -> Set (ETok s)
firstProd = evalDone . firstProd'

firstProd' :: Token s => Prod s -> Done (RId s) () (Set (ETok s))
firstProd' []     = return $ S.singleton Epsilon
firstProd' (x:xs) = do
    fx <- first' x
    case Epsilon `S.member` fx of
        True  -> S.union (S.delete Epsilon fx) <$> firstProd' xs
        False -> return fx

-- | Get all symbols that can follow a rule,
--   also given the start rule and a list of all rules
follow :: Token s => RId s -> RId s -> [RId s] -> Set (Tok s)
follow rid = evalDone `dot` follow' rid

follow' :: Token s => RId s -> RId s -> [RId s] -> Done (RId s) () (Set (Tok s))
follow' rid startrid rids = ifNotDoneG rid (const S.empty) $ do
    putDone rid ()
    (if rid == startrid then S.insert RightEnd else id)
        <$> S.unions
        <$> sequence [followProd prod a
                         | a@(RId _ prods) <- rids
                         , prod <- prods]
  where
    followProd []       _ = return S.empty
    followProd (b:beta) a
        | b == SRule rid = case Epsilon `S.member` firstbeta of
            True  -> (rest `S.union`) <$> follow' a startrid rids
            False -> return rest
        | otherwise      = followProd beta a
      where
        firstbeta = firstProd beta
        rest      = S.map (Tok . unETok) $ S.delete Epsilon firstbeta
