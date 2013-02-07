{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, RecursiveDo #-}
module Ex4StateB (state, Expr, St, evalSt) where

import Control.Applicative
import Control.Monad.Reader
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

-- | The parsing state is just a wrapper around the Reader monad to make it
--   possible to derive Typeable which is needed. We could also use a State
--   monad but that is not necessary in this example.
newtype St a = St { unSt :: Reader [String] a }
  deriving (Typeable, Applicative, Functor, Monad, MonadReader [String])

evalSt = flip runReader [] . unSt

-- | Grammar for the language
state :: Grammar Tok Expr
state = do
  rec
    var   <- rule [ fromTok <@> Var ""]
    term1 <- levels $ do
      rec
        -- Now the rules return 'St' computations instead of their data result.
        t1 <- lrule [ mkLam <@  Lambda <#> var <# RightArrow <#> t1 ]
        t2 <- lrule [ mkApp <@> t2  <#> t3 ]
        t3 <- lrule [ mkVar <@> var
                    , id    <@  LParen <#> t1 <# RParen
                    ]
      return t1
      -- Here we evaluate the final 'St' computation to get the result.
    term <- rule [ evalSt <@> term1 ]
  return term
  where
    mkLam :: String -> St Expr -> St Expr
    mkLam v e = ELam <$> local (v :) e

    mkApp :: St Expr -> St Expr -> St Expr
    mkApp a b = EApp <$> a <*> b

    mkVar :: String -> St Expr
    mkVar v = do
      vars <- ask
      return $ EVar
             $ snd
             $ fromMaybe undefined
             $ find ((== v) . fst) (zip vars [0..])
