{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies  #-}
module Lexer where
import ParserCore
import Control.Applicative
import Data.Char

data Symbol
  = Id String
  | LParen | RParen
  | UScore
  | Arrow
  | HasType
  | Comment
  | LComment | RComment
  | LBrace | RBrace
  | Endl
  | Spc
  deriving (Show, Eq)

class Lex l s | l -> s where
  (-->) :: l -> a -> Parser s a

instance Lex String Char where
  str --> sym = do
    match  str
    return sym

instance Lex [String] Char where
  ss --> sym = oneOf $ map (--> sym) ss

lexer :: Parser Char [Symbol]
lexer = some . oneOf $
  [ "("    --> LParen
  , ")"    --> RParen
  , "_"    --> UScore
  , ["->"] --> Arrow
  , ["::"] --> HasType
  , "--"   --> Comment
  , "{-"   --> LComment
  , "-}"   --> RComment
  , "{"    --> LBrace
  , "}"    --> RBrace
  , "\n"   --> Endl
  , mmunch isSpace >> return Spc
  , Id <$> mmunch (not . isSpace)
  ]

-------------------------------------------------------------------------------
-- Helper functions

match :: Eq s => [s] -> Parser s [s]
match y = do x <- look; m y x
  where 
    m (r:rs) (s:ss) 
      | r == s = do symbol; ss' <- m rs ss; return (s : ss')
    m [] _ = return []
    m _ _  = pfail

munch :: (s -> Bool) -> Parser s [s]
munch r = do s <- look; inspect s
  where
    inspect (c:cs) | r c = do symbol; cs' <- inspect cs; return (c:cs')
    inspect _ = return []

mmunch :: (s -> Bool) -> Parser s [s]
mmunch pred = do
  s <- symbol
  if pred s then do
    ss <- munch pred
    return $ s : ss
   else pfail

oneOf :: [Parser s a] -> Parser s a
oneOf = foldl1 (<|>)
