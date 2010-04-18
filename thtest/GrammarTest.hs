{-# LANGUAGE DoRec #-}
{-# LANGUAGE PackageImports #-}

module GrammarTest where
import Grammar
import LR
--import "monads-fd" Control.Monad.State

{-testGrammar = do
  rec
      x <- (\((a,b),c) -> a:b:c) $::= a ~~ a ~~ x
      y <- (:[])                 $::= "foobar"
      a <- id                    $::= 'a'
  return x -}

testa = do
  rec
    x <- (\(a :~ b) -> a : b)    -= sym 'a' -~ z
                                 -| sym 'b' -~ z
      -| (\([z] :~ x) -> z :~ x) -$ z -~ x
    z <- (:[])                    -= sym 'z'
  return x

{-testFirst t = 
    evalGrammar $ t >>= first

testFollow t =
    evalGrammar $ t >>= follow-}

test f g =
    evalGrammar $ augment g >>= f

e = do
  rec
    x <- id -= a -~ b -~ c
    a <- id -= sym 'a' -| empty
    b <- id -= sym 'b' -| empty
    c <- id -= sym 'c' -| empty
  return x

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Ident String

expr = do
    rec
      e <- id -= mk Add -$ e -~ sym '+' -~ t
                        -| t
      t <- id -= mk Mul -$ t -~ sym '*' -~ f
                        -| f
      f <- id -= par    -$ sym '(' -~ e -~ sym ')'
              -| i      -$ sym 'i'
    return e
  where
    mk c = \(a :~ _ :~ b) -> c a b
    par  = \(_ :~ x :~ _) -> x
    i    = Ident . (:[])

tester = do
    rec
      e <- id -= const 5 -$ f -~ sym 'a' -| const 0 -$ empty
      f <- id -= e
    return e


{-e428 = do
  rec
    e  <- -= t -~ e'
    e' <- -= sym '+' -~ t -~ e'
          -| empty
    t  <- -= f -~ t'
    t' <- -= sym '*' -~ f -~ t
          -| empty
    f <- 
    -}


{-tester = do
  rec
    x <- mkRule $ symbol 'a' -~ symbol 'b' -~ symbol 'c' -$ (\(a :~ b :~ c) -> a:b:c:[])
               -| symbol 'a'                             -$ (\a -> a:[])
                        
  return x
  -}
{-star r = do
  rec
      rest <- star r
      x    <- (:[])       $::= r
      xs   <- uncurry (:) $::= r +++ rule rest
      ret  <- id          $::= x ||| xs
  return ret-}
