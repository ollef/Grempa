{-# LANGUAGE GADTs, DoRec, DeriveDataTypeable, PackageImports, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
module Typed where

import Control.Applicative
import "monads-fd" Control.Monad.State
import Data.Data
import Data.Dynamic
import Data.List
import Data.Maybe

import qualified Data.Set as S
import Data.Set(Set)

type Rule s a = [Prod s a]

proddy :: (Typeable s, Typeable a) => Rule s a -> Int -> Prod s a
proddy = (!!)

-- Inspired by ChristmasTree
-- Will be built backwards for the functions to not be backwards
data Prod s a where
    PSeq  :: Symbol s b -> Prod s (b -> a) -> Prod s a
    PSeqN :: Symbol s b -> Prod s a        -> Prod s a
    PEnd  :: Typeable a => a               -> Prod s a
  deriving Typeable

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
type GRId s a = Grammar s (RId s a)

rule :: (Typeable a, Typeable s) => Rule s a -> GRId s a
rule r = do
    st <- get
    let i : is = ids st
        rid    = RId i r
    put st {ids = is}
    return rid

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

data DynFun = DynFun Dynamic [Bool]

applDynFun :: DynFun -> [Dynamic] -> Dynamic
applDynFun (DynFun f (b:bs)) (a:as)
    | b         = applDynFun (DynFun (dynApp f a) bs) as
    | otherwise = applDynFun (DynFun f bs) as
applDynFun (DynFun f _) _ = f

getFun :: Prod s a -> DynFun
getFun = getFun' []
  where
    getFun' :: [Bool] -> Prod s a -> DynFun
    getFun' as prod = case prod of
        PEnd f    -> DynFun (toDyn f) as
        PSeq  _ p -> getFun' (as ++ [True]) p
        PSeqN _ p -> getFun' (as ++ [False]) p

    app f []     = f
    app f (Just a :as) = app (dynApp f a) as
    app f (Nothing:as) = app f as
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


infixl 5 #>
(#>) :: (ToSym s x, ToSymT s x ~ b)
      => Prod s (b -> a) -> x -> Prod s a
p #> q = PSeq (toSym q) p

infixl 5 #
(#) :: (ToSym s x)
     => Prod s a -> x -> Prod s a
p # q = PSeqN (toSym q) p

infixl 5 <@>
(<@>) :: (ToSym s x, ToSymT s x ~ b, Typeable a, Typeable b)
      => (b -> a) -> x -> Prod s a
f <@> p = PSeq (toSym p) $ PEnd f

infixl 5 <@
(<@) :: (ToSym s x, Typeable a)
     => a -> x -> Prod s a
f <@ p = PSeqN (toSym p) $ PEnd f

sym = STerm
rul = SRule

-----------------------------

data E = E :+: E
       | E :*: E
       | Var
  deriving (Show, Typeable)

e :: GRId Char E
e = do
    rec
      e  <- rule [(:+:) <@> e # '+' #> t
                 ,id    <@> t
                 ]
      t  <- rule [(:*:) <@> t # '*' #> f
                 ,id    <@> f
                 ]
      f  <- rule [id    <@ '(' #> e # ')'
                 ,Var   <@ 'x'
                 ]
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

e1 :: GRId Sym E1
e1 = do
    rec
      e  <- rule [(:++:) <@> e # Plus  #> t
                 ,id     <@> t
                 ]
      t  <- rule [(:**:) <@> t # Times #> f
                 ,id     <@> f
                 ]
      f  <- rule [id     <@ LParen #> e # RParen
                 ,idV    <@> Ident ""
                 ]
    return e
  where
    idV (Ident x) = E1Var x

e1inp = [Ident "x",Times,Ident "y",Times,LParen,Ident "x1",Plus,Ident "y1",RParen]

test :: Grammar Char (RId Char Int)
test = do
    rec
      x <- rule [(\y (Just z) -> y + z) <@> y #> z]
      y <- rule [const 1                <@> '1']
      z <- rule [(Just 3)         <@ '2']
    return x
