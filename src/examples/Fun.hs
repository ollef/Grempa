{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable, DoRec #-}
module Fun
    (Tok(..), lexit, lang)
  where

import Control.Applicative
import Data.Char
import Data.Data
import Data.Typeable
import Language.Haskell.TH.Lift

import Grammar.Typed
import Parser.Static

-- * Lexer for transforming a string into a list of tokens
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
    | isSym   a = go Sym isSym as

testHead f ""    = True
testHead f (a:_) = f a

isId c = isAlphaNum c || c == '_' || c == '\''

isSym '(' = False
isSym ')' = False
isSym c   = isPunctuation c || isSymbol c

go :: (String -> Tok) -> (Char -> Bool) -> String -> [Tok]
go c p xs = let (v, rest) = span p xs in c v : lexit rest

-- * Parser definition

data Def
    = Def String [Pat] Expr
  deriving (Show, Typeable)

data Expr
    = ECase Expr [Branch]
    | ELet Def Expr
    | EApp Expr Expr
    | EVar String
    | ENum Integer
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
num = Num 0

lang :: GRId Tok [Def]
lang = do
  rec
    def <- rule
        [liftA Def fromTok <@>
            var <#> pats <# Equals <#> expr]
    defs <- severalInter SemiColon def
    pat <- rule
        [liftA PCon fromTok <@>
            con <#> pats
        ,id <@> apat
        ]
    apat <- rule
        [paren pat
        ,flip PCon [] . fromTok <@> con
        ,PVar . fromTok         <@> var
        ]
    pats <- several apat
    expr <- rule
        [ECase <@  Case <#> expr <# Of <# LCurl <#> casebrs <# RCurl
        ,ELet  <@  Let  <#> def <# In <#> expr
        ,EApp  <@> expr <#> expr
        ,EVar . fromTok <@> var
        ,ENum . fromNum <@> num
        ,paren expr]
    casebr  <- rule [Branch <@> pat <# RightArrow <#> expr]
    casebrs <- severalInter SemiColon casebr
  return defs
  where
    paren x = id <@ LParen <#> x <# RParen


severalInterR :: (Typeable a, Typeable s)
              => s -> Rule s a -> GRId s [ToSymT s (RId s a)]
severalInterR tok r = rule r >>= severalInter tok

severalInter :: (ToSym s x, ToSymT s x ~ a, Typeable a, Typeable s)
             => s -> x -> GRId s [a]
severalInter tok x = do
  rec
    xs <- rule [epsilon []
               ,(:[]) <@> x
               ,(:)   <@> x <# tok <#> xs]
  return xs

severalR :: (Typeable a, Typeable s)
        => Rule s a -> GRId s [ToSymT s (RId s a)]
severalR r = rule r >>= several

several :: (ToSym s x, ToSymT s x ~ a, Typeable a, Typeable s)
        => x -> GRId s [a]
several x = do
  rec
    xs <- rule [epsilon []
               ,(:) <@> x <#> xs]
  return xs
