{-# LANGUAGE DoRec #-}
import Grammar2

data Symbol
  = LParen
  | RParen
  | Number Int
  | Plus
  | Times

data Expr
  = Expr :+: Expr
  | Expr :*: Expr
  | ENum Int

expr = do
  rec
    e1   <- e2 ~~ Plus ~~ e1 $$ \((e,_),f) -> e :+: f
         |- e2

    e2   <- atom ~~ Times ~~ e2 $$ \((e,_),f) -> e :*: f
         |- atom

    atom <- LParen ~~ e1 ~~ RParen 
         |- Number $$ ENum
  return e1
