{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveDataTypeable, DoRec #-}
module Ex4State (fun, Expr, St, evalSt) where

import Control.Applicative
import Control.Monad.State
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

newtype St a = St { unSt :: State [String] a }
  deriving (Applicative, Functor, Monad, MonadState [String])
deriving instance Typeable1 St

evalSt = flip evalState [] . unSt

-- | Grammar for the language
fun :: Grammar Tok (St Expr)
fun = do
  rec
    var   <- rule [ fromTok <@> Var ""]
    term1 <- rule [ mkLam   <@ Lambda <#> var <# RightArrow <#> term1
                  , id      <@> term2
                  ]
    term2  <- rule [ mkApp  <@> term2 <#> term3
                   , id     <@> term3
                   ]
    term3 <- rule [ mkVar   <@> var
                  , id      <@ LParen <#> term1 <# RParen
                  ]
  return term1
  where
    mkLam :: String -> St Expr -> St Expr
    mkLam v e = do
      modify (v :)
      r <- e
      modify tail
      return $ ELam r

    mkApp :: St Expr -> St Expr -> St Expr
    mkApp a b = EApp <$> a <*> b

    mkVar :: String -> St Expr
    mkVar v = do
      vars <- get
      return $ EVar
             $ snd
             $ fromMaybe undefined
             $ find ((== v) . fst) (zip vars [0..])
