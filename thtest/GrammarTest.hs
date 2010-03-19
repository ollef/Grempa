{-# LANGUAGE DoRec #-}

module GrammarTest where
import Grammar2

{-testGrammar = do
  rec
      x <- (\((a,b),c) -> a:b:c) $::= a ~~ a ~~ x
      y <- (:[])                 $::= "foobar"
      a <- id                    $::= 'a'
  return x -}

testa = do
  rec
    x <- (\(a :~ b) -> a : b)  -::= symbol 'a' -~ rule z  
                                 -| symbol 'b' -~ rule z 
                                 -| symbol 'c' -~ rule z
    z <- id -::= symbol 'z' -$ (\a       -> a:[])
  return x
  
  


tester = do
  rec
    x <- mkRule $ symbol 'a' -~ symbol 'b' -~ symbol 'c' -$ (\(a :~ b :~ c) -> a:b:c:[])
               -| symbol 'a'                             -$ (\a -> a:[])
                        
  return x
{-star r = do
  rec
      rest <- star r
      x    <- (:[])       $::= r
      xs   <- uncurry (:) $::= r +++ rule rest
      ret  <- id          $::= x ||| xs
  return ret-}
