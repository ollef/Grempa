{-# LANGUAGE GADTs, DoRec, DeriveDataTypeable, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Parser.Grempa.Grammar.Typed where

import Control.Monad.State
import Data.Data
import Data.Dynamic

import Data.Parser.Grempa.Parser.Table

type Rule s a = [Prod s a]

-- Inspired by ChristmasTree
-- | A grammar production
--   Will be built backwards for the functions to be "forwards"
data Prod s a where
    PSeq  :: Symbol s b -> Prod s (b -> a) -> Prod s a
    -- Result does not matter
    PSeqN :: Symbol s b -> Prod s a        -> Prod s a
    PEnd  :: Typeable a => a               -> Prod s a
  deriving Typeable

-- | A grammar symbol
data Symbol s a where
    STerm :: s       -> Symbol s s
    SRule :: RId s a -> Symbol s a

-- | Rule ID
data RId s a where
  RId :: (Typeable s, Typeable a)
      => {rId :: RuleI, rIdRule :: Rule s a} -> RId s a
  deriving Typeable

data GrammarState s = GrammarState
    { ids   :: [RuleI]
    }

-- The grammar monad giving a unique RuleI to each new rule
type Grammar s a = State (GrammarState s) a
type GRId s a = Grammar s (RId s a)

-- | Create a new rule for a grammar
rule :: (Typeable a, Typeable s) => Rule s a -> GRId s a
rule r = do
    st <- get
    let i : is = ids st
        rid    = RId i r
    put st {ids = is}
    return rid

-- | Get the result from a Grammar computation
evalGrammar :: Grammar s a -> a
evalGrammar = flip evalState def
  where
    def = GrammarState
        { ids   = [0..]
        }

-- | Create an augmented grammar with a new start symbol
augment :: (Typeable s, Typeable a) => GRId s a -> GRId s a
augment g = do
  rec
    s <- rule [id <@> r]
    r <- g
  return s

getFun :: Prod s a -> DynFun
getFun = getFun' []
  where
    getFun' :: [Bool] -> Prod s a -> DynFun
    getFun' as prod = case prod of
        PEnd f    -> DynFun (toDyn f) as
        PSeq  _ p -> getFun' (True:as) p
        PSeqN _ p -> getFun' (False:as) p

-- | Class for writing grammars in a nicer syntax
class ToSym s a where
  type ToSymT s a :: *
  toSym :: a -> Symbol s (ToSymT s a)

instance ToSym s s where
  type ToSymT s s = s
  toSym = STerm

instance ToSym s (RId s a) where
  type ToSymT s (RId s a) = a
  toSym = SRule

instance ToSym s (Symbol s a) where
  type ToSymT s (Symbol s a) = a
  toSym = id

-- * Combinator functions
-- | Sequence where the result of the symbol to the right matters
infixl 6 <#>
(<#>) :: (ToSym s x, ToSymT s x ~ b)
      => Prod s (b -> a) -> x -> Prod s a
p <#> q = PSeq (toSym q) p

-- | Sequence where the result of the symbol to the right does not matter
infixl 6 <#
(<#) :: (ToSym s x)
     => Prod s a -> x -> Prod s a
p <# q = PSeqN (toSym q) p

-- | Start grammar where the result of the symbol to the right matters
infixl 6 <@>
(<@>) :: (ToSym s x, ToSymT s x ~ b, Typeable a, Typeable b)
      => (b -> a) -> x -> Prod s a
f <@> p = PSeq (toSym p) $ PEnd f

-- | Start grammar where the result of the symbol to the right does not matter
infixl 6 <@
(<@) :: (ToSym s x, Typeable a)
     => a -> x -> Prod s a
f <@ p = PSeqN (toSym p) $ PEnd f

epsilon :: Typeable a => a -> Prod s a
epsilon c = PEnd c
