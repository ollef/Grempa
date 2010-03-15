{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

import qualified Test

testCase = $(Test.mkCase Test.list)

--test = runQ [| list |] >>= prinT



test = runQ [| \n -> case n of
  Symbol "a" -> 1
  Empty -> 2 |] >>= print

testa = runQ (Match (ConP [|Symbol|] [LitP (StringL "a")])) >>= print
