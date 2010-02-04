module Lexer where

import Data.Char

data Symbol
  = Id String
  | Num String
  | Op String
  | LParen
  | RParen
  deriving (Show, Eq)

lexer :: String -> [Symbol]
lexer [] = []
lexer (c:cs)
  | c == '('   = LParen : lexer cs
  | c == ')'   = RParen : lexer cs
  | isLetter c = create isAlphaNum Id
  | isNumber c = create isNumber Num 
  | otherwise  = lexer cs
  where 
    create f constr = let (a,rest) = munch f cs in constr (c:a) : lexer rest
    munch f s@(c:cs) | f c = let (a,rest) = munch f cs in (c : a, rest)
                     | otherwise = ([], s)
    munch _ [] = ([], [])
