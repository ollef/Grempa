{-# LANGUAGE TemplateHaskell, DoRec, DeriveDataTypeable #-}
module Test where

import Control.Monad
import Control.Applicative

import Data.Typeable
import Data.Data
import Language.Haskell.TH.Lift

import Test.QuickCheck

import Data.Parser.Grempa.Static
import Data.Parser.Grempa.Grammar

----------------------------- TEST GRAMMARS -----

data E = E :+: E
       | E :*: E
       | Var
  deriving (Show, Eq, Typeable)

shower x = go (show x)
  where go (' ':cs) = go cs
        go (':':cs) = go cs
        go ('V':'a':'r':cs) = 'x' : go cs
        go (x:cs) = x : go cs
        go [] = []

instance Arbitrary E where
    arbitrary = arb 10
      where
        arb n = frequency
            [ (n, liftM2 (:+:) (arb $ n - 1) (arb $ n - 1))
            , (n, liftM2 (:*:) (arb $ n - 1) (arb $ n - 1))
            , (10, return Var)
            ]

e :: Grammar Char E
e = do
    rec
      e  <- rule [ (:+:) <@> e <# '+' <#> t
                 , id    <@> t
                 ]
      t  <- rule [ (:*:) <@> t <# '*' <#> f
                 , id    <@> f
                 ]
      f  <- rule [ id    <@ '(' <#> e <# ')'
                 , Var   <@ 'x'
                 ]
    return e

data Sym = Ident String
         | Plus
         | Times
         | LParen
         | RParen
  deriving (Eq, Ord, Data, Typeable, Show, Read)

$(deriveLift ''Sym)

instance ToPat Sym where toPat = toConstrPat

data E1 = E1 :++: E1
        | E1 :**: E1
        | E1Var String
  deriving (Eq, Show, Typeable)

e1 :: Grammar Sym E1
e1 = do
    rec
      e  <- rule [ (:++:) <@> e <# Plus  <#> t
                 , id     <@> t
                 ]
      t  <- rule [ (:**:) <@> t <# Times <#> f
                 , id     <@> f
                 ]
      f  <- rule [ id     <@ LParen <#> e <# RParen
                 , idV    <@> Ident ""
                 ]
    return e
  where
    idV (Ident x) = E1Var x
    idV _         = error "idV"

e1inp = [Ident "x",Times,Ident "y",Times,LParen,Ident "x1",Plus,Ident "y1",RParen]

test :: Grammar Char Int
test = do
    rec
      x <- rule [(\y (Just z) -> y + z) <@> y <#> z]
      y <- rule [const 1                <@> '1']
      z <- rule [(Just 3)               <@  '2']
    return x

test2 :: Grammar Char [Char]
test2 = do
    rec
      x <- rule [(\a b x -> a:b:x) <@ 'x' <#> 'a' <#> 'b' <#> x
                ,[]  <@ 'z']
    return x

data S = SAss L R | SR R
  deriving (Eq, Show, Typeable)
data L = Star R | LIdent
  deriving (Eq, Show, Typeable)
data R = R L
  deriving (Eq, Show, Typeable)

{-
S ::=  L = R
    |  R
L ::= *R
    |  x
R ::=  L
-}

ex :: Grammar Char S
ex = do
  rec
    s <- rule [SAss   <@> l <# '=' <#> r
              ,SR     <@> r]
    l <- rule [Star   <@ '*' <#> r
              ,LIdent <@ 'x']
    r <- rule [R      <@> l]
  return s


data Sx = Sx C C deriving (Eq, Show, Typeable)
data C  = Cc C | Cd deriving (Eq, Show, Typeable)

ex454 :: Grammar Char Sx
ex454 = do
  rec
    s <- rule [Sx <@> c  <#> c]
    c <- rule [Cc <@ 'c' <#> c
              ,Cd <@ 'd']
  return s

list :: Grammar Char String
list = do
  rec
    x <- rule [epsilon []
              ,('a':) <@ 'x' <#> x
              ,('b':) <@ 'y' <#> x]
  return x
