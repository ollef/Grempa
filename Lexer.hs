module Lexer where

import Data.Char
import CheckedParser
import Prelude hiding ((>>=), (>>), return)

data Symbol
  = Id String
  | Num String
  | Op String
  | LParen
  | RParen
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Helper functions

pred :: (s -> Bool) -> CParser s (Maybe s)
pred r = do
  s <- symbol
  if r s then return s else pfail

munch :: (s -> Bool) -> CParser s [s]
munch r = do s <- look; inspect s
  where
    inspect (c:cs) | r c = do symbol; cs' <- inspect cs; return (c:cs')
    inspect _ = return []
