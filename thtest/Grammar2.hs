{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}



module Grammar2 where

import Control.Applicative
import Control.Monad.ST
import Data.STRef
import Unsafe.Coerce

infixr 1 -|
infixr 5 :~:
infixr 5 -~
infixr 2 -$
infixr 0 -=

(-~) :: (To Atom s a p, To Seq s b q) => p -> q -> Seq s (Pair a b)
p -~ q = to p :~: to q

(-$) :: To Seq s a p => p -> (a -> b) -> Seq s b
p -$ f = SFun f (to p)

(-|) :: (To Seq s a p, To Rule s a q) => p -> q -> Rule s a
p -| q = Rule (to p : rs)
  where (Rule rs) = to q

(-=) :: To Rule s a p => (a -> b) -> p -> ST t (RId t s b)
f -= r = addRule $ Rule $ map (SFun f) rs
  where (Rule rs) = to r

sym   = ASymbol
rule  = ARule

class To c s a t | c t -> s a where
    to :: t -> c s a

instance To Rule s a (Rule s a) where
    to = id
instance To Rule s a (Seq s a) where
    to = Rule . (:[])
instance To Rule s a (Atom s a) where
    to = to . SOne
instance To Rule s a (RId t s a) where
    to = to . ARule 
--instance To Rule s s s where
    --to = to . symbol

instance To Seq s a (Seq s a) where
    to = id
instance To Seq s a (RId t s a) where
    to = SOne . to
instance To Seq s a (Atom s a) where
    to = SOne . to
--instance To Seq s s s where
    --to = to . symbol

instance To Atom s a (RId t s a) where
    to = ARule
instance To Atom s a (Atom s a) where
    to = id
--instance To Atom s s s where
    --to = symbol

data Rule s a where
    Rule :: [Seq s a] -> Rule s a
  deriving
    Show

unRule (Rule ss) = ss

infixr 5 :~
data Pair a b = a :~ b

data Seq s a where
    (:~:) :: Atom s a -> Seq s b -> Seq s (Pair a b)
    SOne  :: Atom s a -> Seq s a
    SFun   :: (a -> b) -> Seq s a -> Seq s b

instance Show s => Show (Seq s a) where
    show (a :~: as) = show a ++ " " ++ show as
    show (SOne r)   = show r
    show (SFun f r)  = show r

data Atom s a where
    ASymbol :: s         -> Atom s s
    ARule   :: RId t s a -> Atom s a

instance Show s => Show (Atom s a) where
    show (ASymbol s) = show s
    show (ARule _)   = "RULE()"

data AnyRule s where
    AnyRule :: Rule s a -> AnyRule s

data RId t s a = RId (STRef t (AnyRule s))

addRule :: Rule s a -> ST t (RId t s a) 
addRule r = RId <$> newSTRef (AnyRule r)

getRule :: RId t s a -> ST t (Rule s a)
getRule (RId ref) = do
    AnyRule rule <- readSTRef ref
    return $ unsafeCoerce rule

type Grammar s a = forall t. ST t (RId t s a)

evalGrammar :: Grammar s a -> Rule s a
evalGrammar g = runST $ do
    r <- g
    getRule r

mkRule :: To Rule s a p => p -> ST t (RId t s a)
mkRule = addRule . to


items :: Seq s a -> [Integer]
items s = items' 1 s []
  where
    items' :: Integer -> Seq s a -> [Integer] -> [Integer]
    items' n s is = case s of
        _ :~: b  -> items' (n + 1) b (n : is)
        SOne _   -> n + 1 : n : is
        SFun _ p -> items' n p is
