{-# LANGUAGE DoRec #-}

module GrammarTest where
import Grammar2

import Control.Monad.ST

{-testGrammar = do
  rec
      x <- (\((a,b),c) -> a:b:c) $::= a ~~ a ~~ x
      y <- (:[])                 $::= "foobar"
      a <- id                    $::= 'a'
  return x -}

testa = do
  rec
    x <- (\(a :~ b) -> a : b)  -= sym 'a' -~ z
                               -| sym 'b' -~ z
      -| (\([z] :~ x) -> z :~ x) -$ z -~ x
    z <- (:[])                    -= sym 'z'
  return x

test t =  do
    t >>= firstA . ARule

e = do
  rec
    x <- id -= a -~ b -~ c
    a <- id -= sym 'a' -| empty
    b <- id -= sym 'b' -| empty
    c <- id -= sym 'c' -| empty
  return x


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
