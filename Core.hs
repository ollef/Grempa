{-# LANGUAGE GADTs #-}
module Core where

type CTag = String
type Alter b = (CTag, Expr b)

data Expr b -- b is the binder type
  = EVar b
  | EInt Int
  | EConstr CTag Arity -- needed?
  | EAppl (Expr b) (Expr b)
  | ECase (Expr b) [(CTag, Expr b)]
  | ELambda [b] Expr b

