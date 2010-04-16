{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}

module Grammar2 where

import Control.Applicative
import "monads-fd" Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
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

(-=) :: To Rule s a p => (a -> b) -> p -> Grammar s (RId s b)
f -= r = addRule $ Rule $ map (SFun f) rs
  where (Rule rs) = to r

sym   :: s -> Atom s s
sym   = ATerminal . TSymbol
empty :: Atom s s
empty = ATerminal   TEmpty
rule  :: RId s a -> Atom s a
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
    show (SFun _ r) = show r

instance Show s => Show (Atom s a) where
    show (ATerminal t) = show t
    show (ARule _)   = "RULE(_)"

instance Show s => Show (Terminal s) where
    show TEmpty      = "ε"
    show (TSymbol s) = show s

data Any r s where
    Any :: r s a -> Any r s

newtype RId s a = RId Id
type    Id      = Int

(=?=) :: RId s a -> RId s b -> Bool
RId a =?= RId b = a == b

data GrammarState s = GrammarState
    { rules  :: Map Id (Any Rule s)
    , ids    :: [Id]
    }

type Grammar s a = State (GrammarState s) a

data IdRule s a = IdRule (RId s a) (Rule s a)

nextId :: Grammar s Id
nextId = do
    st <- get
    let i = head $ ids st
    put st {ids = tail (ids st)}
    return i

addRule :: Rule s a -> Grammar s (RId s a)
addRule r = do
    i  <- nextId
    st <- get
    put st {rules = M.insert i (Any r) (rules st)}
    return $ RId i

getRule :: RId s a -> Grammar s (Rule s a)
getRule (RId i) = do
    Any r <- fromJust <$> M.lookup i <$> gets rules
    return $ unsafeCoerce r -- should (hopefully) be a safe hack

mkRule :: To Rule s a p => p -> Grammar s (RId s a)
mkRule = addRule . to

getRules :: Grammar s [Any IdRule s]
getRules = map (\(i, Any r) -> Any (IdRule (RId i) r)) <$> M.toList <$> gets rules

evalGrammar :: Grammar s a -> a
evalGrammar = flip evalState def
  where
    def = GrammarState
        { rules = M.empty
        , ids   = [0..]
        }

---------------------------------------------------------------------
first :: (Show s, Ord s) => RId s a -> Grammar s (Set (Terminal s))
first i = do
  r <- getRule i
  firstR r

firstR :: (Show s, Ord s) => Rule s a -> Grammar s (Set (Terminal s))
firstR (Rule ss) = S.unions <$> mapM firstS ss

firstS :: (Show s, Ord s) => Seq s a -> Grammar s (Set (Terminal s))
firstS s = case s of
    SOne a    -> firstA a
    SFun _ sf -> firstS sf
    a :~: ss  -> do
        fas <- firstA a
        let fas' = S.delete TEmpty fas
        if S.member TEmpty fas
            then S.union fas' <$> firstS ss
            else return  fas'

firstA :: (Show s, Ord s) => Atom s a -> Grammar s (Set (Terminal s))
firstA a = case a of
    ARule r -> do
        Rule ss <- getRule r
        S.unions <$> mapM firstS ss
    ATerminal t -> return $ S.singleton t

follow :: (Show s, Ord s) => RId s a -> Grammar s (Set (Terminal s))
follow i = do
    rs <- getRules
    S.unions <$> mapM (followR i) rs

followR :: (Show s, Ord s) => RId s a -> Any IdRule s -> Grammar s (Set (Terminal s))
followR i (Any (IdRule ri (Rule ss))) = do
    b <- or <$> mapM (`endsIn` i) ss
    rest <- if not (ri =?= i) && b then follow ri else return S.empty
    S.union rest <$> S.unions <$> mapM (followS i) ss

endsIn :: (Show s, Ord s) => Seq s a -> RId s b -> Grammar s Bool
endsIn s i = case s of
    SOne (ARule r) | i =?= r -> return True
    SOne _    -> return False
    SFun _ sf -> endsIn sf i
    ARule r :~: ss | i =?= r -> do
        fas <- firstS ss
        if S.member TEmpty fas
            then return True
            else endsIn ss i
    _ :~: ss -> endsIn ss i

followS :: (Show s, Ord s) => RId s a -> Seq s b -> Grammar s (Set (Terminal s))
followS i s = case s of
    SOne _    -> return S.empty
    SFun _ sf -> followS i sf
    ARule r :~: ss | i =?= r -> liftM2 S.union (followS i ss)
                                              (S.delete TEmpty <$> firstS ss)
    _ :~: ss  -> followS i ss

numItems :: Seq s a -> Integer
numItems s = case s of
    SOne _    -> 1
    _ :~: ss  -> 1 + numItems ss
    SFun _ ss ->     numItems ss

items :: Seq s a -> [Integer]
items s = [1..numItems s]

--type Closure s = Map Int (Set (Int, Any Seq s))

