{-# LANGUAGE RankNTypes #-}
-- An implementation of Koen Claessen's Parallel Parser Processes
module Parser
  ( parse
  , symbol
  , pfail
  , look
  , (<|>)
  --, (>>=)
  --, return
  , munch
  ) where

import Data.Char
import Control.Monad

-------------------------------------------------------------------------------

data Parser' s a 
  = SymbolBind (s -> Parser' s a)
  | Fail
  | ReturnChoice a (Parser' s a)
  | LookBind ([s] -> Parser' s a)

(<||>) :: Parser' s a -> Parser' s a -> Parser' s a
SymbolBind f <||> SymbolBind q = SymbolBind (\c -> f c <||> q c)
Fail <||> q    = q
p    <||> Fail = p
ReturnChoice x p <||> q = ReturnChoice x (p <||> q)
p <||> ReturnChoice x q = ReturnChoice x (p <||> q)
LookBind f <||> LookBind q = LookBind (\s -> f s <||> q s)
LookBind f <||> q          = LookBind (\s -> f s <||> q)
p          <||> LookBind q = LookBind (\s -> p <||> q s)

--instance Monad (Parser' s) where
  --return x = ReturnChoice x Fail
  --SymbolBind f >>= k = SymbolBind (\c -> f c >>= k)
  --LookBind   f >>= k = LookBind   (\c -> f c >>= k)
  --Fail         >>= _ = Fail
  --ReturnChoice x p >>= k = k x <||> (p >>= k)

--instance MonadPlus (Parser' s) where
  --mzero = Fail
  --mplus = (<||>)

parse' :: Parser' s a -> [s] -> [(a, [s])]
parse' (SymbolBind x) (c:cs) = parse' (x c) cs
parse' (SymbolBind x)   []   = []
parse' Fail             _    = []
parse' (ReturnChoice x p) s  = (x, s) : parse' p s
parse' (LookBind f) s        = parse' (f s) s

-------------------------------------------------------------------------------
-- Context passing

type Context s a b = a -> Parser' s b
newtype Parser s a = Parser {unParser :: forall b. Context s a b -> Parser' s b}

symbol :: Parser s s
symbol = Parser $ \k -> SymbolBind k

pfail :: Parser s a
pfail = Parser $ \k -> Fail

look :: Parser s [s]
look = Parser $ \k -> LookBind k

(<|>) :: Parser s b -> Parser s b -> Parser s b
Parser p <|> Parser q = Parser $ \k -> p k <||> q k

instance Monad (Parser s) where
  return x = Parser $ \k -> k x
  Parser p >>= f = Parser $ \k -> p $ \x -> unParser (f x) k

parse :: Parser s a -> [s] -> [(a, [s])]
parse (Parser p) = parse' $ p $ flip ReturnChoice Fail

-------------------------------------------------------------------------------
-- Helper functions

munch :: (s -> Bool) -> Parser s [s]
munch r = do s <- look; inspect s
  where
    inspect (c:cs) | r c = do symbol; cs' <- inspect cs; return (c:cs')
    inspect _ = return []
