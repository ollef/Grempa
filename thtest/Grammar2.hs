{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
infixr 0 -::=

(-~) :: (To Atom s a p, To Seq s b q) => p -> q -> Seq s (Pair a b)
p -~ q = to p :~: to q

(-$) :: To Seq s a p => p -> (a -> b) -> Seq s b
p -$ f = SFun f (to p)

(-|) :: (To Seq s a p, To Rule s a q) => p -> q -> Rule s a
p -| q = Rule (to p : rs)
  where (Rule rs) = to q

(-::=) :: To Rule s a p => (a -> b) -> p -> ST t (RId t s b)
f -::= r = addRule $ Rule $ map (SFun f) rs
  where (Rule rs) = to r

symbol = ASymbol
rule   = ARule

class To c s a t | t c -> s a where
    to :: t -> c s a
instance To Seq s a p => To Rule s a p where
    to = Rule . (:[]) . to
instance To Rule s a (Rule s a) where
    to = id
--instance To Rule s a (Atom s a) where
    --to = Rule . (:[]) . to
instance To Atom s a p => To Seq s a p where
    to = SOne . to
instance To Seq s a (Seq s a) where
    to = id
--instance To Seq s a (Atom s a) where
    --to = SOne
instance To Atom s a (RId t s a) where
    to = ARule
instance To Atom s a (Atom s a) where
    to = id
--instance (To c2 s a (c1 s a), To c3 s a (c2 s a)) => To c3 s a (c1 s a) where
    --to = to . to

data Rule s a where
    Rule :: [Seq s a] -> Rule s a
  deriving
    Show

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
    (AnyRule rule) <- readSTRef ref
    return $ unsafeCoerce rule

evalGrammar = runST

mkRule :: To Rule s a p => p -> ST t (RId t s a)
mkRule = addRule . to
