-- | Example 3: A grammar for a small functional language.
--              This example also includes a naive lexer.
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, RecursiveDo #-}

module Ex3Fun (fun, Def) where

import Control.Applicative
import Data.Data

import Data.Parser.Grempa.Grammar

import Ex3FunLex

-- * Result data definitions
data Def
    = Def String [Pat] Expr
  deriving (Eq, Show, Typeable)

data Expr
    = ECase Expr [Branch]
    | ELet Def Expr
    | EApp Expr Expr
    | EOp  Expr String Expr
    | EVar String
    | ENum Integer
    | ECon String
  deriving (Eq, Show, Typeable)

data Branch
    = Branch Pat Expr
  deriving (Eq, Show, Typeable)

data Pat
    = PCon String [Pat]
    | PVar String
  deriving (Eq, Show, Typeable)

-- | Grammar for the language
fun :: Grammar Tok [Def]
fun = do
  rec
    def <- rule
        [Def <$> fromTok
            <@> var <#> pats0 <# Equals <#> expr]
    -- Here we can use the Grempa function 'severalInter0' meaning 0 or more
    -- 'def's interspersed with 'SemiColon's
    defs <- severalInter0 SemiColon def

    pat <- rule
        [PCon <$> fromTok
            <@> con <#> pats
        ,id <@> apat
        ]
    apat <- rule
        [flip PCon [] . fromTok <@> con
        ,PVar . fromTok         <@> var
        ,paren pat
        ]
    -- @pats0@ means 0 or more @apat@s
    pats0 <- several0 apat
    -- This shows the usage of the 'cons' function, which simply creates a new
    -- rule of @apat@ followed by @pats0@, combined with '(:)'.
    pats  <- apat `cons` pats0

    expr <- levels $ do
      rec
        expr <- lrule
            [ECase <@  Case <#> expr <# Of <# LCurl <#> casebrs <# RCurl
            ,ELet  <@  Let  <#> def <# In <#> expr
            ]
        expr1 <- lrule
            -- All binary operators are parsed as being left-associative
            -- A post-processor could be used to change this when fixities
            -- and precedence levels of all operators are are known
            [flip (flip EOp . fromTok)
                   <@> expr1 <#> op <#> expr2
            ]
        expr2 <- lrule
            [EApp  <@> expr2 <#> expr3
            ]
        expr3 <- lrule
            [EVar . fromTok <@> var
            ,ENum . fromNum <@> num
            ,ECon . fromTok <@> con
            ,paren expr
            ]
      return expr

    casebr  <- rule [Branch <@> pat <# RightArrow <#> expr]
    casebrs <- severalInter0 SemiColon casebr

  return defs
  where
    paren x = id <@ LParen <#> x <# RParen
