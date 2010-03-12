{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

import qualified Test

testCase = $(Test.mkCase Test.list)

--test = runQ [| list |] >>= prinT

test = runQ [| \n -> case n of
  1 -> 2
  3 -> 4 |] >>= print

