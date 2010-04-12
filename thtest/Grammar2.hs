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
import Data.IORef
import Data.Map(Map)
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S
import Unsafe.Coerce

infixr 1 -|
infixr 5 :~:
infixr 5 -~
infixr 2 -$
infixr 0 -=

(-~) :: (To Atom s a p, To Seq s b q) => p -> q -> Seq s (Pair a b)
p -~ q = to p :~: to q

(-$) :: To Seq s a p => (a -> b) -> p -> Seq s b
f -$ p = SFun f (to p)

(-|) :: (To Seq s a p, To Rule s a q) => p -> q -> Rule s a
p -| q = Rule (to p : rs)
  where (Rule rs) = to q

(-=) :: To Rule s a p => (a -> b) -> p -> Grammar s b
f -= r = addRule $ Rule $ map (SFun f) rs
  where (Rule rs) = to r

sym   = ATerminal . TSymbol
empty = ATerminal   TEmpty
rule  = ARule

class To c s a t | c t -> s a where
    to :: t -> c s a

instance To Rule s a (Rule s a) where
    to = id
instance To Rule s a (Seq s a) where
    to = Rule . (:[])
instance To Rule s a (Atom s a) where
    to = to . SOne
instance To Rule s a (RId s a) where
    to = to . ARule
--instance To Rule s s s where
    --to = to . symbol

instance To Seq s a (Seq s a) where
    to = id
instance To Seq s a (RId s a) where
    to = SOne . to
instance To Seq s a (Atom s a) where
    to = SOne . to
--instance To Seq s s s where
    --to = to . symbol

instance To Atom s a (RId s a) where
    to = ARule
instance To Atom s a (Atom s a) where
    to = id
--instance To Atom s s s where
    --to = symbol

---------------------------------------------------------------------
data Rule s a where
    Rule :: [Seq s a] -> Rule s a
  deriving Show

unRule (Rule ss) = ss

infixr 5 :~
data Pair a b = a :~ b

data Seq s a where
    (:~:) :: Atom s a -> Seq s b -> Seq s (Pair a b)
    SOne  :: Atom s a -> Seq s a
    SFun  :: (a -> b) -> Seq s a -> Seq s b

data Atom s a where
    ATerminal :: Terminal s -> Atom s s
    ARule     :: RId s a    -> Atom s a

data Terminal s where
    TEmpty  ::      Terminal s
    TSymbol :: s -> Terminal s
  deriving (Ord, Eq)

instance Show s => Show (Seq s a) where
    show (a :~: as) = show a ++ " " ++ show as
    show (SOne r)   = show r
    show (SFun f r)  = show r

instance Show s => Show (Atom s a) where
    show (ATerminal t) = show t
    show (ARule _)   = "RULE(_)"

instance Show s => Show (Terminal s) where
    show TEmpty      = "ε"
    show (TSymbol s) = show s

data Any r s where
    Any :: r s a -> Any r s

data RId s a = RId (IORef (Any Rule s))

addRule :: Rule s a -> IO (RId s a)
addRule r = do
    RId <$> newIORef (Any r)

getRule :: RId s a -> IO (Rule s a)
getRule (RId ref) = do
    Any rule <- readIORef ref
    return $ unsafeCoerce rule

type Grammar s a = IO (RId s a)

--evalGrammar :: Grammar s a -> Rule s a
--evalGrammar g = runST $ g >>= getRule

mkRule :: To Rule s a p => p -> Grammar s a
mkRule = addRule . to

---------------------------------------------------------------------
firstS :: (Show s, Ord s) => Seq s a -> IO (Set (Terminal s))
firstS s = case s of
    SOne a   -> firstA a
    SFun f s -> firstS s
    a :~: ss -> do
        fas <- firstA a
        let fas' = S.delete TEmpty fas
        if S.member TEmpty fas
            then S.union fas' <$> firstS ss
            else return  fas'

firstA :: (Show s, Ord s) => Atom s a -> IO (Set (Terminal s))
firstA a = case a of
    ARule r -> do
        Rule ss <- getRule r
        S.unions <$> mapM firstS ss
    ATerminal t -> return $ S.singleton t

numItems :: Seq s a -> Integer
numItems s = case s of
    SOne _   -> 1
    _ :~: s  -> 1 + numItems s
    SFun _ s ->     numItems s

items :: Seq s a -> [Integer]
items s = [1..numItems s]

--type Closure s = Map Int (Set (Int, Any Seq s))

