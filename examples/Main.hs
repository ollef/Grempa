{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.Environment

import Text.Grempa.Parser.Static
import Text.Grempa.Parser.Dynamic
import Text.Grempa.Grammar.Typed

import Fun
-- We also need the token datatype in scope or Template Haskell will whine
import Lex

parse = $(mkStaticParser lang [|lang|])
--parse = evalGrammar . runSLRC lang

main = print . lexAndParse =<< readFile . head =<< getArgs

lexAndParse = parse . lexit
