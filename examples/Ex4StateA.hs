{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveDataTypeable, DoRec #-}
module Ex4StateA (state, Expr, St, evalSt) where

import Control.Applicative
import Control.Monad.State hiding (state)
import Data.Data
import Data.List
import Data.Maybe

import Data.Parser.Grempa.Grammar

import Ex4StateLex

-- * Result data definitions
data Expr
    = EApp Expr Expr
    | EVar Integer
    | ELam Expr
  deriving (Eq, Show, Typeable)

type St a = [String] -> a
evalSt f = f []

-- | Grammar for the language
state :: Grammar Tok Expr
state = do
  rec
    var   <- rule [ fromTok <@> Var ""]
    term1 <- levels $ do
      rec
        -- Here the rules return functions of type 'St'.
        t1 <- lrule [ mkLam <@  Lambda <#> var <# RightArrow <#> t1 ]
        t2 <- lrule [ mkApp <@> t2  <#> t3 ]
        t3 <- lrule [ mkVar <@> var
                    , id    <@  LParen <#> t1 <# RParen
                    ]
      return t1
      -- Here we apply the final 'St' function to get the real result.
    term <- rule [ evalSt <@> term1 ]
  return term
  where
    mkLam :: String -> St Expr -> St Expr
    mkLam v e st = ELam (e (v : st))

    mkApp :: St Expr -> St Expr -> St Expr
    mkApp a b st = EApp (a st) (b st)

    mkVar :: String -> St Expr
    mkVar v vars = EVar $ snd
                        $ fromMaybe undefined
                        $ find ((== v) . fst) (zip vars [0..])
