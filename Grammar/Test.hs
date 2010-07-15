{-# LANGUAGE TemplateHaskell, DoRec, DeriveDataTypeable #-}
module Test where

import Data.Typeable
import Data.Data
import Language.Haskell.TH.Lift

import StaticParser
import Typed



----------------------------- TEST GRAMMARS -----

data E = E :+: E
       | E :*: E
       | Var
  deriving (Show, Typeable)

e :: GRId Char E
e = do
    rec
      e  <- rule [(:+:) <@> e <# '+' <#> t
                 ,id    <@> t
                 ]
      t  <- rule [(:*:) <@> t <# '*' <#> f
                 ,id    <@> f
                 ]
      f  <- rule [id    <@ '(' <#> e <# ')'
                 ,Var   <@ 'x'
                 ]
    return e

data Sym = Ident String
         | Plus
         | Times
         | LParen
         | RParen
  deriving (Eq, Ord, Data, Typeable, Show, Read)

instance ConstrTok Sym where
$(deriveLift ''Sym)

data E1 = E1 :++: E1
        | E1 :**: E1
        | E1Var String
  deriving (Show, Typeable)

e1 :: GRId Sym E1
e1 = do
    rec
      e  <- rule [(:++:) <@> e <# Plus  <#> t
                 ,id     <@> t
                 ]
      t  <- rule [(:**:) <@> t <# Times <#> f
                 ,id     <@> f
                 ]
      f  <- rule [id     <@ LParen <#> e <# RParen
                 ,idV    <@> Ident ""
                 ]
    return e
  where
    idV (Ident x) = E1Var x
    idV _         = error "idV"

e1inp = [Ident "x",Times,Ident "y",Times,LParen,Ident "x1",Plus,Ident "y1",RParen]

test :: Grammar Char (RId Char Int)
test = do
    rec
      x <- rule [(\y (Just z) -> y + z) <@> y <#> z]
      y <- rule [const 1                <@> '1']
      z <- rule [(Just 3)               <@  '2']
    return x

test2 :: Grammar Char (RId Char [Char])
test2 = do
    rec
      x <- rule [(\a b x -> a:b:x) <@ 'x' <#> 'a' <#> 'b' <#> x
                ,[]  <@ 'z']
    return x

data S = SAss L R | SR R
  deriving (Show, Typeable)
data L = Star R | LIdent
  deriving (Show, Typeable)
data R = R L
  deriving (Show, Typeable)

ex :: GRId Char S
ex = do
  rec
    s <- rule [SAss   <@> l <# '=' <#> r
              ,SR     <@> r]
    l <- rule [Star   <@ '*' <#> r
              ,LIdent <@ 'x']
    r <- rule [R      <@> l]
  return s

data Sx = Sx C C deriving (Show, Typeable)
data C  = Cc C | Cd deriving (Show, Typeable)

ex454 :: GRId Char Sx
ex454 = do
  rec
    s <- rule [Sx <@> c  <#> c]
    c <- rule [Cc <@ 'c' <#> c
              ,Cd <@ 'd']
  return s

list :: GRId Char [Char]
list = do
  rec
    x <- rule [epsilon []
              ,('a':) <@ 'x' <#> x
              ,('b':) <@ 'y' <#> x]
  return x
