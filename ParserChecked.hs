{-# LANGUAGE NoImplicitPrelude, GADTs, EmptyDataDecls, FunctionalDependencies, MultiParamTypeClasses, IncoherentInstances, FlexibleInstances #-}
module ParserChecked
  ( CParser
  , Monad
  , parse
  , symbol
  , pfail
  , look
  , (<|>)
  , (>>=)
  , (>>)
  , return
  , fail
  ) where

import qualified Prelude as P
--import Prelude hiding (Monad, (>>=), (>>), return)

import qualified ParserCore as PCore

-------------------------------------------------------------------------------
-- Type-level booleans

data True
data False
{-
class Or a b c | a b -> c
instance Or True True True
instance Or True False True
instance Or False True True
instance Or False False False

class And a b c | a b -> c
instance And True True True
instance And True False False
instance And False True False
instance And False False False-}

data Parser s c a where
  Symbol :: Parser s True s
  Fail   :: Parser s False a
  Return :: a -> Parser s False a
  Choice :: {-And c1 c2 c3 =>-} Parser s c1 a -> Parser s c2 a -> Parser s c1 a
  Bind   :: {-And c1 c2 c3 =>-} Parser s c1 a -> (a -> Parser s c2 b) -> Parser s c1 b
  Look   :: Parser s False [s]

-- True parsers are parsers that are guaranteed to eat some of the input
type CParser s a = Parser s True a

infixl 1 >>, >>=

class Monad m where
  (>>=)  :: {-(And c1 c2 c3) =>-} m c1 a -> (a -> m c2 b) -> m c1 b
  (>>)   :: {-(And c1 c2 c3) =>-} m c1 a -> m c2 b -> m c1 b
  return :: a -> m False a
  fail   :: P.String -> m False a
  p >> q = p >>= \_ -> q

instance Monad (Parser s) where
  (>>=)  = Bind
  return = Return
  fail s = Fail

toCore :: Parser s c a -> PCore.Parser s a
toCore p = case p of
  Symbol     -> PCore.symbol
  Fail       -> PCore.pfail
  Return x   -> P.return x
  Choice p q -> toCore p PCore.<|> toCore q
  Bind p f   -> toCore p P.>>= toCore P.. f
  Look       -> PCore.look

symbol = Symbol
pfail = Fail
infix 0 <|>
--(<|>) :: And c1 c2 c3 => Parser s c1 a -> Parser s c2 a -> Parser s c3 a
(<|>) = Choice
look = Look

parse :: Parser s True a -> [s] -> [(a, [s])]
parse = PCore.parse . toCore
