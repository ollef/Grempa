{-# LANGUAGE DoRec #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Grammar2

instance To Atom Symbol Symbol Symbol where
    to s = symbol s

--instance To Seq Symbol Symbol Symbol where
    --to s = SOne (symbol s)

--instance To Rule Symbol Symbol Symbol where
    --to s = Rule [to s]

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
    e1   <- id -::= e2     -~ Plus  -~ e1     -$ (\(e :~ _ :~ f) -> e :+: f)
                 -| e2
    e2   <- id -::= atom   -~ Times -~ e2     -$ (\(e :~ _ :~ f) -> e :*: f)
                 -| atom
    atom <- id -::= LParen -~ e1    -~ RParen -$ (\(_ :~ e :~ _) -> e)
                 -| (Number 1)                -$ (\(Number n) -> ENum n)
  return e1
