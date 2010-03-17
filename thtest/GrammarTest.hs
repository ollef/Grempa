{-# LANGUAGE DoRec #-}

import Control.Applicative
import Grammar

testGrammar = do
  rec
      x <- (\((a,b),c) -> a:b:c) $::= a ~~ a ~~ x
      y <- (:[])                 $::= "foobar"
      a <- id                    $::= 'a'
  return x

{-testa = do
  rec
    x <- 'a' ~~ z  $$ \(a,b) -> a:b
    x |- 'b' ~~ z  $$ \(a,b) -> a:b
    x |- 'c' ~~ z  $$ \(a,b) -> a:b
    z <- 'z'       $$ \a -> a:[] -}

{-star r = do
  rec
      rest <- star r
      x    <- (:[])       $::= r
      xs   <- uncurry (:) $::= r +++ rule rest
      ret  <- id          $::= x ||| xs
  return ret-}
