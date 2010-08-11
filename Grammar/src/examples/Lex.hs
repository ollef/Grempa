{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DoRec #-}
module Lex
    (Tok(..), lexit, lang)
  where

import Control.Applicative
import Data.Char
import Data.Data
import Data.Typeable
import Language.Haskell.TH.Lift

import Grammar.Typed
import Parser.Static

data Tok
    = Var String
    | Con String
    | Sym String
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

fromTok :: Tok -> String
fromTok (Var s) = s
fromTok (Con s) = s
fromTok (Sym s) = s

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
    | isSym   a = go Sym isSym as

testHead f ""    = True
testHead f (a:_) = f a

isId c = isAlphaNum c || c == '_' || c == '\''

isSym '(' = False
isSym ')' = False
isSym c   = isPunctuation c || isSymbol c

go :: (String -> Tok) -> (Char -> Bool) -> String -> [Tok]
go c p xs = let (v, rest) = span p xs in c v : lexit rest

-- Parser definition

data Def
    = Def String [Pat] Expr
  deriving (Show, Typeable)

data Expr
    = ECase Expr [Branch]
    | ELet Branch Expr
    | EApp Expr Expr
  deriving (Show, Typeable)

data Branch
    = Branch Pat Expr
  deriving (Show, Typeable)

data Pat
    = PCon String [Pat]
    | PVar String
  deriving (Show, Typeable)

var = Var ""
con = Con ""
sym = Sym ""

lang :: GRId Tok [Def]
lang = do
  rec
    defs  <- severalInter SemiColon $ rule
        [liftA Def  fromTok <@>
            var <#> pats <# Equals <#> expr]
    pat <- paren $ rule
        [liftA PCon fromTok <@>
            con <#> pats
        ,liftA PVar fromTok <@>
            var
        ]
    pats <- several $ rule [id <@> pat]
    expr <- paren $ rule
        [ECase <@  Case <#> expr <# Of <#> brs
        ,ELet  <@  Let  <#> br <# In <#> expr
        ,EApp  <@> expr <#> expr]
    br   <- rule [Branch <@> pat <# RightArrow <#> expr]
    brs' <- severalInter SemiColon $ rule [id <@> br]
    brs  <- rule [id <@ LCurl <#> brs' <# RCurl]
  return defs

severalInter :: (Typeable a, Typeable t) => t -> GRId t a -> GRId t [a]
severalInter tok r = do
  rec
    x  <- r
    xs <- rule [epsilon []
               ,(:) <@> x <# tok <#> xs]
  return xs

several :: (Typeable a, Typeable t) => GRId t a -> GRId t [a]
several r = do
  rec
    x  <- r
    xs <- rule [epsilon []
               ,(:) <@> x <#> xs]
  return xs


paren :: Typeable a => GRId Tok a -> GRId Tok a
paren r = do
  rec
    x <- r
    p <- rule [id <@ LParen <#> x <# RParen
              ,id <@> x]
  return p
