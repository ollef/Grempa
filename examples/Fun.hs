{-# LANGUAGE TypeFamilies, DeriveDataTypeable, DoRec #-}
module Fun (lang, Def)
  where

import Control.Applicative
import Data.Data

import Data.Parser.Grempa.Grammar

import Lex

-- * Result data definitions
data Def
    = Def String [Pat] Expr
  deriving (Show, Typeable)

data Expr
    = ECase Expr [Branch]
    | ELet Def Expr
    | EApp Expr [Expr]
    | EOp  Expr String Expr
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

-- | Grammar for the language
lang :: GRId Tok [Def]
lang = do
  rec
    def <- rule
        [Def <$> fromTok
            <@> var <#> pats <# Equals <#> expr]
    defs <- severalInter SemiColon def

    pat <- rule
        [PCon <$> fromTok
            <@> con <#> pats
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
        ,id    <@> expr1
        ]
    expr1 <- rule
        -- All binary operators are parsed as being left-associative
        -- A post-processor could be used to change this when fixities
        -- and precedence levels of all operators are are known
        [flip (flip EOp . fromTok)
               <@> expr1 <#> op <#> expr2
        ,id    <@> expr2
        ]
    expr2 <- rule
        [EApp  <@> expr2 <#> expr3s
        ,id    <@> expr3
        ]
    expr3 <- rule
        [EVar . fromTok <@> var
        ,ENum . fromNum <@> num
        ,paren expr
        ]
    expr3s  <- several expr3

    casebr  <- rule [Branch <@> pat <# RightArrow <#> expr]
    casebrs <- severalInter SemiColon casebr

  return defs
  where
    paren x = id <@ LParen <#> x <# RParen


-- * Helper functions

-- | Create a new rule which consists of any number of the argument rule
--   example: @several rule@ matches @rule rule ... rule@
several :: (ToSym s x, ToSymT s x ~ a, Typeable a, Typeable s)
        => x -> GRId s [a]
several x = do
  rec
    xs <- rule [epsilon []
               ,(:) <@> x <#> xs]
  return xs

-- | Create a new rule which consists of a list interspersed with a token,
--   example: @severalInter ';' rule@ matches @rule ';' rule ';' ... ';' rule@
severalInter :: (ToSym s x, ToSymT s x ~ a, Typeable a, Typeable s)
             => s -> x -> GRId s [a]
severalInter tok x = do
  rec
    xs <- rule [epsilon []
               ,(:[]) <@> x
               ,(:)   <@> x <# tok <#> xs]
  return xs
