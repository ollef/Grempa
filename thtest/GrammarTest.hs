{-# LANGUAGE DoRec #-}
import Grammar

testGrammar = do
  rec
      x <- (\(a, y) -> a : y) ->::= rule a +++ rule y
      y <- (:[])              ->::= symbol 'b'
      a <- mkRule $ symbol 'a'
  return y

