{-# LANGUAGE DoRec #-}
import Grammar2

data Symbol
  = LParen
  | RParen
  | Number Int
  | Plus
  | Times
  deriving Show

data Expr
  = Expr :+: Expr
  | Expr :*: Expr
  | ENum Int

expr :: Grammar Symbol Expr
expr = do
  rec
    e1   <- id -= e2   -~ sym Plus  -~ e1 -$ (\(e :~ _ :~ f) -> e :+: f)
               -| e2
    e2   <- id -= atom -~ sym Times -~ e2 -$ (\(e :~ _ :~ f) -> e :*: f)
               -| atom
    atom <- id -= sym LParen -~ e1 -~ sym RParen -$ (\(_ :~ e :~ _) -> e)
               -| sym (Number 1) -$ (\(Number n) -> ENum n)
  return e1
