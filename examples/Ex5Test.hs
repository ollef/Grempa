-- | Use QuickCheck to test your parsers.
module Ex4Test where

-- The Grempa library has to be built with the test flag to be able to use this.
import Data.Parser.Grempa.Test
import Test.QuickCheck

-- Import the different parsers and grammars
import Ex1SimpleExpr(expr)
import Ex1SimpleExprParser(parseExprStatic)

import Ex2Calculator(calc)
import Ex2CalculatorParser(parseCalcStatic)

import Ex3Fun(fun)
import Ex3FunParser(parseFunStatic)

-- | Running 'quickCheck' on these different tests will generate random
--   inputs from the grammars and the expected output, and compare the parser's
--   output with that. This is useful to see that the parser covers all of the
--   defined language (you should get conflicts if it does not, but it feels
--   good to get an assurance).
testEx1, testEx2, testEx3 :: Property
testEx1 = prop_parser parseExprStatic  expr
testEx2 = prop_parser parseCalcStatic  calc
testEx3 = prop_parser parseFunStatic   fun
