{-# LANGUAGE DoRec #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UndecidableInstances #-}

module Grammar2 where

import Control.Applicative
import "monads-fd" Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as S
import Unsafe.Coerce

infixr 1 -|
infixr 5 :~:
infixr 5 -~
infixr 2 -$
infixr 0 -=

(-~) :: (To Atom s a p, To Seq s b q) => p -> q -> Seq s (Pair a b)
p -~ q = to p :~: to q

(-$) :: To Seq s a p => (a -> b) -> p -> Seq s b
f -$ p = SFun f (to p)

(-|) :: (To Seq s a p, To Rule s a q) => p -> q -> Rule s a
p -| q = Rule (to p : rs)
  where (Rule rs) = to q

(-=) :: (Show s, To Rule s a p) => (a -> b) -> p -> Grammar s (RId s b)
f -= r = addRule $ Rule $ map (SFun f) rs
  where (Rule rs) = to r

sym   :: s -> Atom s s
sym   = ATerminal . TSymbol
empty :: Atom s s
empty = ATerminal   TEmpty
rule  :: RId s a -> Atom s a
rule  = ARule

class To c s a t | c t -> s a where
    to :: t -> c s a

instance To Rule s a (Rule s a) where
    to = id
instance To Rule s a (Seq s a) where
    to = Rule . (:[])
instance To Rule s a (Atom s a) where
    to = to . SOne
instance To Rule s a (RId s a) where
    to = to . ARule
--instance To Rule s s s where
    --to = to . symbol

instance To Seq s a (Seq s a) where
    to = id
instance To Seq s a (RId s a) where
    to = SOne . to
instance To Seq s a (Atom s a) where
    to = SOne . to
--instance To Seq s s s where
    --to = to . symbol

instance To Atom s a (RId s a) where
    to = ARule
instance To Atom s a (Atom s a) where
    to = id
--instance To Atom s s s where
    --to = symbol

---------------------------------------------------------------------
data Rule s a = Rule
    { unRule :: [Seq s a] }
  deriving Show

infixr 5 :~
data Pair a b = a :~ b

data Seq s a where
    (:~:) :: Atom s a -> Seq s b -> Seq s (Pair a b)
    SOne  :: Atom s a -> Seq s a
    SFun  :: (a -> b) -> Seq s a -> Seq s b

data Atom s a where
    ATerminal :: Terminal s -> Atom s s
    ARule     :: RId s a    -> Atom s a

data Terminal s where
    TEmpty  ::      Terminal s
    TSymbol :: s -> Terminal s
  deriving (Ord, Eq)

instance Show s => Show (Seq s a) where
    show (a :~: as) = show a ++ " " ++ show as
    show (SOne r)   = show r
    show (SFun _ r) = show r

instance Show s => Show (Atom s a) where
    show (ATerminal t) = show t
    show (ARule _)   = "RULE(_)"

instance Show s => Show (Terminal s) where
    show TEmpty      = "ε"
    show (TSymbol s) = show s

data Any t where
    Any :: t a -> Any t

--instance Show (r s a) => Show (Any r s) where
    --show (Any x) = show x

newtype RId s a = RId Id
  deriving Show
type    Id      = Int

(=?=) :: RId s a -> RId s b -> Bool
RId a =?= RId b = a == b

data GrammarState s = GrammarState
    { rules  :: Map (Any (RId s)) (Any (Rule s))
    , ids    :: [Id]
    }

type Grammar s a = State (GrammarState s) a

data IdRule s a = IdRule (RId s a) (Rule s a)
  deriving Show

nextId :: Grammar s Id
nextId = do
    st <- get
    let i = head $ ids st
    put st {ids = tail (ids st)}
    return i

addRule :: Show s => Rule s a -> Grammar s (RId s a)
addRule r = do
    i  <- nextId
    st <- get
    put st {rules = M.insert (Any (RId i)) (Any r) (rules st)}
    return $ RId i

getRule :: RId s a -> Grammar s (Rule s a)
getRule i = do
    Any r <- fromJust <$> M.lookup (Any i) <$> gets rules
    return $ unsafeCoerce r -- should be a safe hack

mkRule :: (Show s, To Rule s a p) => p -> Grammar s (RId s a)
mkRule = addRule . to

--getRules :: Show s => Grammar s [Any (IdRule s)]
--getRules = map (\(i, Any r) -> Any (IdRule (RId i) r)) <$> M.toList <$> gets rules

evalGrammar :: Grammar s a -> a
evalGrammar = flip evalState def
  where
    def = GrammarState
        { rules = M.empty
        , ids   = [0..]
        }

--rid :: Id -> Rule s a -> RId s a
--rid i = RId i

---------------------------------------------------------------------

instance Eq (Any (RId s)) where
    Any (RId i) == Any (RId j) = i == j
instance Ord (Any (RId s)) where
    Any (RId i) `compare` Any (RId j) = i `compare` j
instance Show (Any (RId s)) where
    show (Any (RId i)) = show i

instance Eq s => Eq (Any (Atom s)) where
    Any (ATerminal x) == Any (ATerminal y) = x == y
    Any (ARule x)     == Any (ARule y)     = Any x == Any y
    _                 == _                 = False
instance Ord s => Ord (Any (Atom s)) where
    Any (ATerminal x) `compare` Any (ATerminal y) = x `compare` y
    Any (ARule x)     `compare` Any (ARule y)     = Any x `compare` Any y
    Any (ATerminal _) `compare` _                 = LT
    Any (ARule _)     `compare` _                 = GT
instance Show s => Show (Any (Atom s)) where
    show (Any (ATerminal x)) = show x
    show (Any (ARule x))     = show x

data Item s = Item
    { itRule :: Any (RId s)
    , itProd :: Int
    , itPos  :: Int
    }
  deriving (Show, Eq, Ord)

augment :: Show s => Grammar s (RId s a) -> Grammar s (RId s a)
augment g = do
  rec
    s <- id -= r
    r <- g
  return s

items :: Ord s => RId s a -> Grammar s (Set (Set (Item s)))
items i = do
    cl <- closureR i
    gsyms <- S.toList <$> grammarSymbols
    items' gsyms S.empty $ S.singleton cl
  where
    items' :: Ord s => [Any (Atom s)] -> Set (Set (Item s)) -> Set (Set (Item s))
           -> Grammar s (Set (Set (Item s)))
    items' gsyms done c = do
        let itemSets = S.toList c
        gotos <- S.fromList <$> sequence [goto is x | is <- itemSets, x <- gsyms]
        let c'    = gotos S.\\ (S.insert S.empty done)
            done' = done `S.union` c
        case S.null c' of
            True  -> return done
            False -> items' gsyms done' c'

grammarSymbols :: Ord s => Grammar s (Set (Any (Atom s)))
grammarSymbols = do
    rs <- gets rules
    let ts   = S.unions
             $ concatMap (\(Any (Rule ss)) -> map grammarSymbolsS ss) (M.elems rs)
        nts  = S.fromList
             $ map (\(Any i) -> Any (ARule i)) (M.keys rs)
    return $ ts `S.union` nts
  where
    grammarSymbolsS :: Ord s => Seq s a -> Set (Any (Atom s))
    grammarSymbolsS s = case s of
        SOne a    -> grammarSymbolsA a
        a :~: ss  -> grammarSymbolsA a `S.union` grammarSymbolsS ss
        SFun _ ss -> grammarSymbolsS ss
    grammarSymbolsA :: Ord s => Atom s a -> Set (Any (Atom s))
    grammarSymbolsA a = case a of
        ATerminal (TSymbol _) -> S.singleton (Any a)
        _                     -> S.empty

closureR :: RId s a      -> Grammar s (Set (Item s))
closureR i = closure (S.singleton $ Item (Any i) 0 0)

closure :: Set (Item s) -> Grammar s (Set (Item s))
closure its = closure' S.empty its
  where
    closure' done is = do
        let is' = is S.\\ done
        case S.null is' of
            True -> return done
            False -> do
                let done' = done `S.union` is'
                is'' <- S.unions <$> mapM closItem (S.toList is')
                closure' done' is''

    closItem i = do
        a <- nextAtom i
        case a of
            Any (ARule r) -> firstItems r
            _             -> return S.empty

    firstItems :: RId s a -> Grammar s (Set (Item s))
    firstItems i = do
        Rule ss <- getRule i
        let si = take (length ss) [0..]
        return $ S.fromList $ map (\n -> Item (Any i) n 0) si

goto :: Eq s => Set (Item s) -> Any (Atom s) -> Grammar s (Set (Item s))
goto si at = do
    let its = S.toList si
    g <- catMaybes <$> mapM (nextTest at) its
    closure $ S.fromList g
  where
    nextTest :: Eq s => Any (Atom s) -> Item s -> Grammar s (Maybe (Item s))
    nextTest x i = do
        a <- nextAtom i
        if x == a
            then return $ Just i {itPos = itPos i + 1}
            else return Nothing

nextAtom :: Item s -> Grammar s (Any (Atom s))
nextAtom (Item (Any r) prod pos) = do
    Rule ss <- getRule r
    return $ findNextS (ss !! prod) pos
  where
    findNextS :: Seq s a -> Int -> Any (Atom s)
    findNextS (ATerminal TEmpty :~: ss) n = findNextS ss n
    findNextS (SFun _ ss)               n = findNextS ss n
    findNextS (SOne a)                  0 = Any a
    findNextS (a :~: _)                 0 = Any a
    findNextS (SOne _)                  _ = Any $ ATerminal TEmpty
    findNextS (_ :~: ss)                n = findNextS ss (n - 1)
