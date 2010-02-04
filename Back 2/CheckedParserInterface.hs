{-# LANGUAGE GADTs, KindSignatures, ExistentialQuantification #-}
module CheckedParserInterface where

import Control.Monad

import qualified CheckedParser as CP
import qualified ParserCore    as PC

data Parser s a where
  Symbol :: Parser s s
  Fail   :: Parser s a
  Return :: a -> Parser s a
  Choice :: Parser s a -> Parser s a -> Parser s a
  Bind   :: Parser s a -> (a -> Parser s b) -> Parser s b
  Look   :: Parser s [s]

instance Monad (Parser s) where
  return = Return
  (>>=)  = Bind

instance MonadPlus (Parser s) where
  mzero = Fail
  mplus = Choice

toCP :: Parser s a -> CP.CheckedP s a
toCP Symbol       = CP.CheckedP CP.Symbol
toCP Fail         = CP.CheckedP CP.Fail
toCP (Return x)   = CP.CheckedP $ CP.Return x
toCP (Choice p q) = CP.CheckedP $ CP.Choice (toCP p) (toCP q)
toCP (Bind p q)   = CP.CheckedP $ CP.Bind (toCP p) (toCP . q)
toCP Look         = CP.CheckedP CP.Look

test p = case toCP p of
  CP.CheckedP p -> CP.convertToParser p
