import Data.Parser.Grempa.Test.Arb
import Data.Parser.Grempa.Grammar.Untyped

import Test

import Test.QuickCheck

import Fun

import Data.Parser.Grempa.Grammar.Typed

testa x = sample $ arb 5 (fst $ unType id $ evalGrammar x)
