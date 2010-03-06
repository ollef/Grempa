{-# LANGUAGE RankNTypes #-}
-- An implementation of Koen Claessen's Parallel Parser Processes
module ParserCore
  ( Parser
  , parse
  , symbol
  , pfail
  , look
  , (<|>)
  , munch
  , disjoint
  , try
  ) where

import Control.Applicative
import Control.Monad
import Data.Char

import Unsafe.Coerce

-------------------------------------------------------------------------------

data Parser' s a 
  = SymbolBind (s -> Parser' s a)
  | Fail
  | ReturnChoice a (Parser' s a)
  | LookBind ([s] -> Parser' s a)
  | TryBind (Parser' s a) (Maybe a -> Parser' s a)
  -- | Disjoint (Parser' s a) (Parser' s a)

(<||>) :: Parser' s a -> Parser' s a -> Parser' s a
SymbolBind f <||> SymbolBind q = SymbolBind (\c -> f c <||> q c)
Fail <||> q    = q
p    <||> Fail = p
ReturnChoice x p <||> q = ReturnChoice x (p <||> q)
p <||> ReturnChoice x q = ReturnChoice x (p <||> q)
LookBind f <||> LookBind q = LookBind (\s -> f s <||> q s)
LookBind f <||> q          = LookBind (\s -> f s <||> q)
p          <||> LookBind q = LookBind (\s -> p   <||> q s)
TryBind p f <||> q = TryBind (p <||> q) (\x -> f x <||> q)
p <||> TryBind q f = TryBind (p <||> q) (\x -> p <||> f x)

--Disjoint p q <||> Disjoint r s = foldr1 Disjoint [p, q, r, s]
--Disjoint p q <||> r = Disjoint (p <||> r) (q <||> r)
p <||> q = error $ show p ++ " <||> " ++ show q 

instance Show (Parser' s a) where
  show (SymbolBind p) = "SymbolBind "
  show Fail = "Fail "
  show (ReturnChoice a p) = "ReturnChoice " ++ show p
  show (LookBind f) = "LookBind "
  --show (Disjoint p1 p2) = "Disjoint " ++ show p1 ++ show p2
  show (TryBind q s) = "TryBind " ++ show q

parse' :: Parser' s a -> [s] -> [(a, [s])]
parse' (SymbolBind x) (c:cs) = parse' (x c) cs
parse' (SymbolBind x)   []   = []
parse' Fail             _    = []
parse' (ReturnChoice x p) s  = if null s then (x, s) : rest else rest
  where rest = parse' p s
parse' (LookBind f) s        = parse' (f s) s
parse' (TryBind p f) s       = if null res then 
  where res = parse' p
--parse' (Disjoint p q) s      = if null res1 then res2 else res1
  --where 
    --res1 = parse' p s
    --res2 = parse' q s

-------------------------------------------------------------------------------
-- Context passing

type Context s a b = a -> Parser' s b
newtype Parser s a = Parser {unParser :: forall b. Context s a b -> Parser' s b}

symbol :: Parser s s
symbol = Parser $ \k -> SymbolBind k

pfail :: Parser s a
pfail = mzero

look :: Parser s [s]
look = Parser $ \k -> LookBind k

try :: Parser s a -> Parser s (Maybe a)
try p = Parser $ \k -> TryBind k

--disjoint :: Parser s a -> Parser s a -> Parser s a 
--disjoint p q = Parser $ \k -> Disjoint (unParser p k) (unParser q k)

instance Monad (Parser s) where
  return x = Parser $ \k -> k x
  Parser p >>= f = Parser $ \k -> p $ \x -> unParser (f x) k

instance MonadPlus (Parser s) where
  mzero = Parser $ \k -> Fail
  Parser p `mplus` Parser q = Parser $ \k -> p k <||> q k

instance Functor (Parser s) where
  fmap f p = p >>= (return . f)

instance Applicative (Parser s) where
  pure  = return
  (<*>) = ap

instance Alternative (Parser s) where
  empty = mzero
  (<|>) = mplus

parse :: Parser s a -> [s] -> [(a, [s])]
parse (Parser p) = parse' $ p $ flip ReturnChoice Fail
-------------------------------------------------------------------------------
-- Helper functions
munch :: (s -> Bool) -> Parser s [s]
munch r = do s <- look; inspect s
  where
    inspect (c:cs) | r c = do symbol; cs' <- inspect cs; return (c:cs')
    inspect _ = return []
