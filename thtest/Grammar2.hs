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

--import Debug.Trace
trace c m = m

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
data Rule s a where
    Rule :: [Seq s a] -> Rule s a
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
    { rules  :: Map Id (Any (Rule s))
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
    put st {rules = M.insert i (Any r) (rules st)}
    return $ RId i

getRule :: RId s a -> Grammar s (Rule s a)
getRule (RId i) = do
    Any r <- fromJust <$> M.lookup i <$> gets rules
    return $ unsafeCoerce r -- should be a safe hack

mkRule :: (Show s, To Rule s a p) => p -> Grammar s (RId s a)
mkRule = addRule . to

getRules :: Show s => Grammar s [Any (IdRule s)]
getRules = map (\(i, Any r) -> Any (IdRule (RId i) r)) <$> M.toList <$> gets rules

evalGrammar :: Grammar s a -> a
evalGrammar = flip evalState def
  where
    def = GrammarState
        { rules = M.empty
        , ids   = [0..]
        }

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

data Item s = Item
    { itRule :: Any (RId s)
    , itProd :: Int
    , itPos  :: Int
    }
  deriving (Show, Eq, Ord)

settt = S.fromList [item 0 0 0, item 1 0 1] S.\\ S.fromList [item 1 0 0, item 2 0 2]
  where
    item x y z = Any $ RId x

augment :: Grammar s a -> Grammar s a
augment g = do
  rec
    s <- id -= r
    r <- g
  return s

{-items :: RId s a -> Grammar s [Item s]
items i = do
    Rule ss <- getRule i
    let ps = zip ss [0..]
    return $ concatMap prods ps
  where
    prods (s, n) = map (\x -> Item (Any i) n x) (itemsS s)
    itemsS s = [0..numItemsS s]
    numItemsS :: Seq s a -> Int
    numItemsS s = case s of
        SOne (ATerminal TEmpty) -> 0
        SOne _                  -> 1
        a :~: ss  -> numItemsS (SOne a) + numItemsS ss
        SFun _ ss -> numItemsS ss
        -}

clos    :: Show s => RId s a      -> Grammar s (Set (Item s))
clos i = closure (S.singleton $ Item (Any i) 0 0)

closure :: Set (Item s) -> Grammar s (Set (Item s))
closure si = closure' S.empty si
  where
    closure' done si = do
        let si' = si S.\\ done
        case S.null si' of
            True -> return done
            False -> do
                let items = S.toList si'
                    done' = done `S.union` si'
                si'' <- S.unions <$> mapM closItem items
                closure' done' si''

    closItem i = do
        a <- nextAtom i
        case a of
            Any (ARule i) -> firstItems i
            _             -> return S.empty

    firstItems :: RId s a -> Grammar s (Set (Item s))
    firstItems i = do
        Rule ss <- getRule i
        let si = zip ss [0..]
        return $ S.fromList $ map (\(s, n) -> Item (Any i) n 0) si

goto :: Eq s => Set (Item s) -> Atom s a -> Grammar s (Set (Item s))
goto si x = do
    let items = S.toList si
    g <- catMaybes <$> mapM (nextTest (Any x)) items
    closure $ S.fromList g
  where
    nextTest :: Eq s => Any (Atom s) -> Item s -> Grammar s (Maybe (Item s))
    nextTest x i = do
        a <- nextAtom i
        if x == a
            then return $ Just i {itPos = itPos i + 1}
            else return Nothing

nextAtom :: Item s -> Grammar s (Any (Atom s))
nextAtom (Item (Any rule) prod pos) = do
    Rule ss <- getRule rule
    return $ findNextS (ss !! prod) pos
  where
    findNextS :: Seq s a -> Int -> Any (Atom s)
    findNextS (ATerminal TEmpty :~: ss) n = findNextS ss n
    findNextS (SFun _ ss)               n = findNextS ss n
    findNextS s 0 = case s of
        SOne a    -> Any a
        a :~: _   -> Any a
    findNextS s n = case s of
        SOne a    -> Any $ ATerminal TEmpty
        _ :~: ss  -> findNextS ss (n - 1)
