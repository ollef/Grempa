{-# LANGUAGE GADTs, EmptyDataDecls, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances #-}
module CheckedParser
  ( CParser
  , parse
  , symbol
  , pfail
  , look
  , (<|>)
  , (>>=)
  , return
  ) where

import qualified Prelude as P
import Prelude hiding (Monad, (>>=), (>>), return)

import qualified ParserCore as PC

data True
data False

class Or a b c | a b -> c

instance Or True b True
instance Or a True True
instance Or False False False

class And a b c | a b -> c

instance And False b False
instance And a False False
instance And True True True

data Parser s c a where
  Symbol :: Parser s True s
  Fail   :: Parser s False a
  Return :: a -> Parser s False a
  Choice :: And c1 c2 c3 => Parser s c1 a -> Parser s c2 a -> Parser s c3 a
  Bind   :: Or c1 c2 c3 => Parser s c1 a -> (a -> Parser s c2 b) -> Parser s c3 b
  Look   :: Parser s False [s]

type CParser s a = Parser s True a

infixl 1 >>, >>=

class Monad m where
  (>>=)  :: (Or c1 c2 c3) => m c1 a -> (a -> m c2 b) -> m c3 b
  (>>)   :: (Or c1 c2 c3) => m c1 a -> m c2 b -> m c3 b
  return :: a -> m False a
  p >> q = p >>= \_ -> q

instance Monad (Parser s) where
  (>>=) = Bind
  return = Return

toPC :: Parser s c a -> PC.Parser s a
toPC p = case p of
  Symbol     -> PC.symbol
  Fail       -> PC.pfail
  Return x   -> P.return x
  Choice p q -> toPC p PC.<|> toPC q
  Bind p f   -> toPC p P.>>= toPC . f
  Look       -> PC.look

symbol = Symbol
pfail = Fail
(<|>) :: And c1 c2 c3 => Parser s c1 a -> Parser s c2 a -> Parser s c3 a
(<|>) = Choice
look = Look

parse :: Parser s True a -> [s] -> [(a, [s])]
parse = PC.parse . toPC

failer = symbol >> failer
