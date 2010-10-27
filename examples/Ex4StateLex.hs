{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Ex4StateLex
    ( Tok(..), lexToks
    ) where

import Data.Char
import Data.Data
import Language.Haskell.TH.Lift
import Data.Parser.Grempa.Static

-- | Token datatype
data Tok
    = Var {fromTok :: String}
    | Lambda
    | RightArrow
    | LParen | RParen
  deriving (Eq, Ord, Data, Typeable, Show, Read)

$(deriveLift ''Tok)
instance ToPat Tok where toPat = toConstrPat

-- | Do the lexing!
lexToks :: String -> [Tok]
lexToks [] = []
lexToks ('-':'>'        :as) | testHead (not . isSym) as = RightArrow : lexToks as
lexToks ('\\'            :as) = Lambda : lexToks as
lexToks ('('            :as) = LParen : lexToks as
lexToks (')'            :as) = RParen : lexToks as
lexToks as@(a:rest)
    | isLower a = go Var isId as
    | otherwise = lexToks rest

go :: (String -> Tok) -> (Char -> Bool) -> String -> [Tok]
go c p xs = let (v, rest) = span p xs in c v : lexToks rest

testHead :: (Char -> Bool) -> String -> Bool
testHead _ ""    = True
testHead f (a:_) = f a

isId :: Char -> Bool
isId c = isAlphaNum c || c == '_' || c == '\''

isSym :: Char -> Bool
isSym '(' = False
isSym ')' = False
isSym c   = isPunctuation c || isSymbol c
