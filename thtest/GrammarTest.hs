{-# LANGUAGE DoRec #-}
import Control.Applicative
import Grammar

testGrammar = do
  rec
      x <- (\((a, b), c) -> a:b:c) $::= rule a +++ rule a +++ rule x
      y <- (:[])                   $::= symbol 'b'
      a <- id                      $::= symbol 'a'
  return x

star r = do
  rec
      rest <- star r
      x    <- (:[])       $::= r
      xs   <- uncurry (:) $::= r +++ rule rest
      ret  <- id          $::= x ||| xs
  return ret
