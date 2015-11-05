{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
-- Needed to derive ToSym Tok Tok
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

-- A lexer for Example 3. This is a really naive lexer and should not be used
-- in production. It is merely for showing how the parser works.
module Ex3FunLex
    ( Tok(..), lexToks
    , var, con, op, num
    ) where

import Data.Char
import Data.Data
import Language.Haskell.TH.Lift
import Data.Parser.Grempa.Static
import Data.Parser.Grempa.Grammar (ToSym(..), Symbol(STerm))

-- | Token datatype
data Tok
    = Var {fromTok :: String}
    | Con {fromTok :: String}
    | Op  {fromTok :: String}
    | Data
    | Case | Of
    | Let  | In
    | Num {fromNum :: Integer}
    | Equals
    | RightArrow
    | LParen | RParen
    | LCurl  | RCurl
    | SemiColon
    | Bar
  deriving (Eq, Ord, Data, Typeable, Show, Read)

$(deriveLift ''Tok)
instance ToPat Tok where toPat = toConstrPat

instance ToSym Tok Tok where
  type ToSymT Tok Tok = Tok
  toSym = STerm

-- * Shorthands for constructors applied to something
--   (could be anything since the ToPat instance creates wildcard patterns for
--    everything save for the constructor)
var, con, op, num :: Tok
var = Var ""
con = Con ""
op  = Op  ""
num = Num 0

-- | Do the lexing!
lexToks :: String -> [Tok]
lexToks [] = []
lexToks ('d':'a':'t':'a':as) | testHead (not . isId)  as = Data   : lexToks as
lexToks ('c':'a':'s':'e':as) | testHead (not . isId)  as = Case   : lexToks as
lexToks ('o':'f'        :as) | testHead (not . isId)  as = Of     : lexToks as
lexToks ('l':'e':'t'    :as) | testHead (not . isId)  as = Let    : lexToks as
lexToks ('i':'n'        :as) | testHead (not . isId)  as = In     : lexToks as
lexToks ('='            :as) | testHead (not . isSym) as = Equals : lexToks as
lexToks ('-':'>'        :as) | testHead (not . isSym) as = RightArrow : lexToks as
lexToks ('|'            :as) | testHead (not . isSym) as = RParen : lexToks as
lexToks ('('            :as) = LParen : lexToks as
lexToks (')'            :as) = RParen : lexToks as
lexToks ('{'            :as) = LCurl  : lexToks as
lexToks ('}'            :as) = RCurl  : lexToks as
lexToks (';'            :as) = SemiColon  : lexToks as
lexToks as@(a:rest)
    | isLower a = go Var isId as
    | isUpper a = go Con isId as
    | isDigit a = go (Num . read) isDigit as
    | isSym   a = go Op isSym as
    | otherwise = lexToks rest

testHead :: (Char -> Bool) -> String -> Bool
testHead _ ""    = True
testHead f (a:_) = f a

isId :: Char -> Bool
isId c = isAlphaNum c || c == '_' || c == '\''

isSym :: Char -> Bool
isSym '(' = False
isSym ')' = False
isSym c   = isPunctuation c || isSymbol c

go :: (String -> Tok) -> (Char -> Bool) -> String -> [Tok]
go c p xs = let (v, rest) = span p xs in c v : lexToks rest
