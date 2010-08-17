{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.Environment

import Parser.Static
import Parser.Dynamic
import Grammar.Typed

import Fun

parse = $(mkStaticParser lang [|lang|])
--parse = evalGrammar . runSLRC lang

main = print . lexAndParse =<< readFile . head =<< getArgs

lexAndParse = parse . lexit
