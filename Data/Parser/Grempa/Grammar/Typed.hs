{-# LANGUAGE GADTs, DoRec, DeriveDataTypeable, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Parser.Grempa.Grammar.Typed
    ( Grammar
    , Prod(..), Symbol(..), RId(..)
    , GrammarState
    , rule
    , evalGrammar
    , augment
    , getFun
    , ToSym(..)
    , (<@>), (<@)
    , (<#>), (<#)
    , epsilon) where

import Control.Monad.State
import Data.Data
import Data.Dynamic

import Data.Parser.Grempa.Parser.Table

type Rule t a = [Prod t a]

-- Inspired by ChristmasTree
-- | A grammar production
data Prod t a where
    -- Sequence a production and a symbol.
    PSeq  :: Prod t (b -> a) -> Symbol t b -> Prod t a
    -- Sequence where the result of the symbol does not matter.
    PSeqN :: Prod t a -> Symbol t b        -> Prod t a
    -- The semantic action combining a production into a result.
    PFun  :: Typeable a => a               -> Prod t a
  deriving Typeable

-- | A grammar symbol
data Symbol t a where
    -- A terminal (token).
    STerm :: t       -> Symbol t t
    -- A reference to a grammar rule.
    SRule :: RId t a -> Symbol t a

-- | Rule ID
data RId s a where
  RId :: (Typeable t, Typeable a)
      => {rId :: RuleI, rIdRule :: Rule t a} -> RId t a
  deriving Typeable

-- The grammar monad giving a unique RuleI to each new rule
newtype RuleIDs t = RuleIDs { rules :: [RuleI] }
type GrammarState t a = State (RuleIDs t) a
type Grammar t a = GrammarState t (RId t a)

-- | Get the result from a Grammar computation
evalGrammar :: GrammarState t a -> a
evalGrammar = flip evalState (RuleIDs [0..])

-- | Create an augmented grammar (with a new start symbol)
augment :: (Typeable t, Typeable a) => Grammar t a -> Grammar t a
augment g = do
  rec
    s <- rule [id <@> r]
    r <- g
  return s

-- | Get the semantic action from a production
getFun :: Prod t a -> DynFun
getFun = getFun' []
  where
    getFun' :: [Bool] -> Prod s a -> DynFun
    getFun' as prod = case prod of
        PFun  f   -> DynFun  (toDyn  f) as
        PSeq  p _ -> getFun' (True :as) p
        PSeqN p _ -> getFun' (False:as) p

-- | Create a new rule in a grammar
rule :: (Typeable a, Typeable t) => Rule t a -> Grammar t a
rule r = do
    st <- get
    let i:is = rules st
    put st {rules = is}
    return $ RId i r

-- | Class for writing grammars in a nicer syntax.
--   This class allows one to use both rules and tokens with the grammar
--   combinator functions. For the grammars to typecheck, it is often necessary
--   to give their type.
class ToSym t a where
  type ToSymT t a :: *
  toSym :: a -> Symbol t (ToSymT t a)

instance ToSym t t where
  type ToSymT t t = t
  toSym = STerm

instance ToSym t (RId t a) where
  type ToSymT t (RId t a) = a
  toSym = SRule

instance ToSym t (Symbol t a) where
  type ToSymT t (Symbol t a) = a
  toSym = id

-- * Combinator functions
-- | Sequence a production and a grammar symbol, where the symbol directly to
--   the right of the operator is used in the semantic action.
infixl 3 <#>
(<#>) :: (ToSym t x, ToSymT t x ~ b)
      => Prod t (b -> a) -> x -> Prod t a
p <#> q = PSeq p $ toSym q

-- | Sequence a production and a grammar symbol, where the symbol directly to
--   the right of the operator is not used in the semantic action.
infixl 3 <#
(<#) :: ToSym t x
     => Prod t a -> x -> Prod t a
p <# q = PSeqN p $ toSym q

-- | Start a production, where the symbol directly to the right of the operator
--   is used in the semantic action.
infixl 3 <@>
(<@>) :: (ToSym t x, ToSymT t x ~ b, Typeable a, Typeable b)
      => (b -> a) -- ^ The semantic action function for the production
      -> x        -- ^ A grammar symbol
      -> Prod t a
f <@> p = PSeq (PFun f) $ toSym p

-- | Start a production, where the symbol directly to the right of the operator
--   is not used in the semantic action.
infixl 3 <@
(<@) :: (ToSym t x, Typeable a)
     => a -- ^ The semantic action function for the production
     -> x -- ^ A grammar symbol
     -> Prod t a
f <@ p = PSeqN (PFun f) $ toSym p

-- | The empty production, taking the semantic action (in this case just the
--   value to return) as the argument.
epsilon :: Typeable a => a -> Prod t a
epsilon c = PFun c
