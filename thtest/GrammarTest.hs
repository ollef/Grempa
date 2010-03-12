{-# LANGUAGE DoRec #-}
import Control.Applicative
import Grammar

testGrammar = do
  rec
      x <- (\(a, y) -> a : y) $::= rule a +++ rule x
      y <- (:[])              $::= symbol 'b'
      a <- id                 $::= symbol 'a'
  return x

