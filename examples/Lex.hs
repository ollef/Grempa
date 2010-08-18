{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Lex where

import Data.Char
import Data.Data
import Data.Typeable
import Language.Haskell.TH.Lift
import Text.Grempa.Parser.Static

-- * Lexer for transforming a string into a list of tokens
data Tok
    = Var String
    | Con String
    | Op String
    | Data
    | Case | Of
    | Let  | In
    | Num Integer
    | Equals
    | RightArrow
    | LParen | RParen
    | LCurl  | RCurl
    | SemiColon
    | Bar
  deriving (Eq, Ord, Data, Typeable, Show, Read)

-- * Shorthands for constructors applied to something
--   (could be anything since the ToPat instance creates wildcard patterns for
--    everything save for the constructor)
var = Var ""
con = Con ""
op  = Op  ""
num = Num 0

fromTok :: Tok -> String
fromTok (Var s) = s
fromTok (Con s) = s
fromTok (Op  s) = s

fromNum :: Tok -> Integer
fromNum (Num n) = n

$(deriveLift ''Tok)

instance ToPat Tok where toPat = toConstrPat

lexit :: String -> [Tok]
lexit [] = []
lexit ('d':'a':'t':'a':as) | testHead (not . isId)  as = Data   : lexit as
lexit ('c':'a':'s':'e':as) | testHead (not . isId)  as = Case   : lexit as
lexit ('o':'f'        :as) | testHead (not . isId)  as = Of     : lexit as
lexit ('l':'e':'t'    :as) | testHead (not . isId)  as = Let    : lexit as
lexit ('i':'n'        :as) | testHead (not . isId)  as = In     : lexit as
lexit ('='            :as) | testHead (not . isSym) as = Equals : lexit as
lexit ('-':'>'        :as) | testHead (not . isSym) as = RightArrow : lexit as
lexit ('|'            :as) | testHead (not . isSym) as = RParen : lexit as
lexit ('('            :as) = LParen : lexit as
lexit (')'            :as) = RParen : lexit as
lexit ('{'            :as) = LCurl  : lexit as
lexit ('}'            :as) = RCurl  : lexit as
lexit (';'            :as) = SemiColon  : lexit as
lexit as@(a:rest)
    | isSpace a = lexit rest
    | isLower a = go Var isId as
    | isUpper a = go Con isId as
    | isDigit a = go (Num . read) isDigit as
    | isSym   a = go Op isSym as

testHead f ""    = True
testHead f (a:_) = f a

isId c = isAlphaNum c || c == '_' || c == '\''

isSym '(' = False
isSym ')' = False
isSym c   = isPunctuation c || isSymbol c

go :: (String -> Tok) -> (Char -> Bool) -> String -> [Tok]
go c p xs = let (v, rest) = span p xs in c v : lexit rest
