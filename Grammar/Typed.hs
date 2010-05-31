{-# LANGUAGE GADTs, DoRec, PackageImports #-}
module Typed where

import Control.Applicative
import "monads-fd" Control.Monad.State

type Rule s a = [Prod s a]

-- Inspired by ChristmasTree
data Prod s a where
    PSeq :: Symbol s b -> Prod s (b -> a) -> Prod s a
    PEnd :: a -> Prod s a

instance Functor (Prod s) where
    fmap f (PSeq s p) = PSeq s $ fmap (f . ) p
    fmap f (PEnd x)   = PEnd $ f x

--instance Applicative (Prod s) where
    --pure x = PEnd x
    --PSeq s p <*> q = PSeq s (p <*> q)
    --PSeq s p <*> q        = PSeq s (PSeq p q)

data Symbol s a where
    STerm :: s       -> Symbol s s
    SRule :: RId s a -> Symbol s a

data RId s a = RId {rId :: Int, rIdRule :: Rule s a}

data GrammarState s = GrammarState
    { ids   :: [Int]
    }

type Grammar s a = State (GrammarState s) a

addRule :: Rule s a -> Grammar s (RId s a)
addRule rule = do
    st <- get
    let i : is = ids st
        rid    = RId i rule
    put st {ids = is}
    return rid

evalGrammar :: Grammar s a -> a
evalGrammar = flip evalState def
  where
    def = GrammarState
        { ids   = [0..]
        }

infixr 5 .#.
infixr 5 .$.
(.#.) = PSeq
p .$. f = PSeq p (PEnd f)

infixr 4 .|.
(.|.) = (:)

sym = STerm
rule = SRule

-----------------------------
data E = Add E E
       | Mul E E
       | Var

e = do
    rec
      e  <- addRule [rule e .#. sym '+' .#. rule t .$. \x _ y -> Add x y
                    ,rule t .$. id]
      t  <- addRule [rule t .#. sym '*' .#. rule f .$. \x _ y -> Mul x y
                    ,rule f .$. id]
      f  <- addRule [sym '(' .#. rule e .#. sym ')' .$. \_ e _ -> e
                    ,sym 'x' .$. const Var]
    return e
