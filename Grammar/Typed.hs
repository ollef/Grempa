{-# LANGUAGE GADTs, DoRec, DeriveDataTypeable, PackageImports #-}
module Typed where

import Control.Applicative
import "monads-fd" Control.Monad.State
import Data.Data
import Data.Dynamic
import Data.Function
import Data.List
import Data.Maybe

import qualified Data.Set as S
import Data.Set(Set)

type Rule s a = [Prod s a]

proddy :: (Typeable s, Typeable a) => Rule s a -> Int -> Prod s a
proddy = (!!)

-- Inspired by ChristmasTree
data Prod s' a' where
    PSeq :: Symbol s b -> Prod s (b -> a) -> Prod s a
    PEnd :: Typeable a
         => a -> Prod s a
  deriving Typeable

getFun :: Prod s a -> Dynamic
getFun (PSeq _ p) = getFun p
getFun (PEnd f)   = toDyn f

getRuleProdFun :: Int -> Int -> RId s a -> Dynamic
getRuleProdFun rn p = fromJust . flip evalState S.empty . getRule'
  where
    getRule' :: RId s a -> State (Set Int) (Maybe Dynamic)
    getRule' rid@(RId i r) = if i == rn
        then return $ Just $ (getFun $ rIdRule rid !! p)
        else do
            done <- get
            case i `S.member` done of
                True  -> return Nothing
                False -> do
                  put $ S.insert i done
                  Just <$> head <$> catMaybes <$> mapM getRuleP r
    getRuleP :: Prod s a -> State (Set Int) (Maybe Dynamic)
    getRuleP (PSeq s p) = case s of
        SRule rid -> do
            x <- getRule' rid
            case x of
                Just x  -> return $ Just x
                Nothing -> getRuleP p
        _         -> getRuleP p
    getRuleP _          = return Nothing

data Symbol s a where
    STerm :: s       -> Symbol s s
    SRule :: RId s a -> Symbol s a

data RId s a where
  RId :: (Typeable s, Typeable a)
      => {rId :: Int, rIdRule :: Rule s a} -> RId s a
  deriving Typeable

data GrammarState s = GrammarState
    { ids   :: [Int]
    }

type Grammar s a = State (GrammarState s) a

addRule :: (Typeable a, Typeable s) => Rule s a -> Grammar s (RId s a)
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

-- | Type for representing symbols only caring about the constructor
data CSym a where
    CSym :: {unCSym :: a} -> CSym a
  deriving (Show, Typeable)

instance Data a => Eq (CSym a) where
    CSym x == CSym y = ((==) `on` toConstr) x y

instance (Data a, Ord a) => Ord (CSym a) where
    CSym x `compare` CSym y = case ((==) `on` toConstr) x y of
        True  -> EQ
        False -> x `compare` y

infixr 5 .#.
infixr 5 .$.
(.#.) = PSeq
p .$. f = PSeq p (PEnd f)

sym = STerm
rule = SRule

-----------------------------
data E = E :+: E
       | E :*: E
       | Var
  deriving (Show, Typeable)

e :: Grammar Char (RId Char E)
e = do
    rec
      e  <- addRule [rule e .#. sym '+' .#. rule t .$. \x _ y -> x :+: y
                    ,rule t .$. id]
      t  <- addRule [rule t .#. sym '*' .#. rule f .$. \x _ y -> x :*: y
                    ,rule f .$. id]
      f  <- addRule [sym '(' .#. rule e .#. sym ')' .$. \_ e _ -> e
                    ,sym 'x' .$. const Var]
    return e

data Sym = Ident String
         | Plus
         | Times
         | LParen
         | RParen
  deriving (Eq, Ord, Data, Typeable, Show, Read)

data E1 = E1 :++: E1
        | E1 :**: E1
        | E1Var String
  deriving (Show, Typeable)

e1 :: Grammar Sym (RId Sym E1)
e1 = do
    rec
      e  <- addRule [rule e .#. sym Plus  .#. rule t .$. \x _ y -> x :++: y
                    ,rule t .$. id]
      t  <- addRule [rule t .#. sym Times .#. rule f .$. \x _ y -> x :**: y
                    ,rule f .$. id]
      f  <- addRule [sym LParen .#. rule e .#. sym RParen .$. \_ e _ -> e
                    ,sym (Ident "") .$. \(Ident x) -> E1Var x]
    return e

e1inp = [Ident "x",Times,Ident "y",Times,LParen,Ident "x1",Plus,Ident "y1",RParen]
