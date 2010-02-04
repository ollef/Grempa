{-# LANGUAGE RankNTypes #-}
-- An implementation of Koen Claessen's Parallel Parser Processes
module ParserCore
  ( Parser
  , parse
  , symbol
  , pfail
  , look
  , (<|>)
  --, munch
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
pfail = mzero

look :: Parser s [s]
look = Parser $ \k -> LookBind k

infixr 5 <|>
(<|>) :: Parser s b -> Parser s b -> Parser s b
(<|>) = mplus

instance Monad (Parser s) where
  return x = Parser $ \k -> k x
  Parser p >>= f = Parser $ \k -> p $ \x -> unParser (f x) k

instance MonadPlus (Parser s) where
  mzero = Parser $ \k -> Fail
  Parser p `mplus` Parser q = Parser $ \k -> p k <||> q k

parse :: Parser s a -> [s] -> [(a, [s])]
parse (Parser p) = parse' $ p $ flip ReturnChoice Fail

-------------------------------------------------------------------------------
-- Helper functions

{-munch :: (s -> Bool) -> Parser s [s]
munch r = do s <- look; inspect s
  where
    inspect (c:cs) | r c = do symbol; cs' <- inspect cs; return (c:cs')
    inspect _ = return [] -}
