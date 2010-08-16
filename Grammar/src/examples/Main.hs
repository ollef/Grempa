{-# LANGUAGE TemplateHaskell #-}
module Main where

import Parser.Static
import Parser.Dynamic
import Grammar.Typed

import Lex

parse = $(mkStaticParser lang [|lang|])
--parse = evalGrammar . runSLRC lang

main = print $ lexAndParse "f (Con x) = case g (x y) z of {XX z -> majs;ZZ y -> f y;}"

lexAndParse = parse . lexit
