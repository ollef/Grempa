{-# LANGUAGE DoRec #-}

import Control.Applicative
import Grammar

testGrammar = do
  rec
      x <- (\((a,b),c) -> a:b:c) $::= a ~~ a ~~ x
      y <- (:[])                 $::= "foobar"
      a <- id                    $::= 'a'
  return x

{-star r = do
  rec
      rest <- star r
      x    <- (:[])       $::= r
      xs   <- uncurry (:) $::= r +++ rule rest
      ret  <- id          $::= x ||| xs
  return ret-}

r = do
  rec
    a <- id $::= (\(a,b) -> a ++ b:[]) $: a ~~ 'a' -|- (:[]) $: 'b'
  return a

left = do
    p' <- r >>= leftMostG
    return p'
