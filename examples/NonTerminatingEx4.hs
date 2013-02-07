{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveDataTypeable, RecursiveDo #-}
module NonTerminatingEx4 (state, Expr, St, evalSt) where

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
state :: Grammar Tok Expr
state = do
  rec
    var   <- rule [ fromTok <@> Var ""]
    aterm <- rule [ mkVar  <@> var
                  , id     <@ LParen <#> term <# RParen
                  ]
    term  <- rule [ mkLam  <@  Lambda <#> var <# RightArrow <#> aterm
                  , mkApp  <@> term <#> term
                  , id     <@> aterm
                  ]
    res <- rule [evalSt <@> term]
  return res
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
