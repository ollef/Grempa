{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.Environment

import Data.Parser.Grempa.Static
import Data.Parser.Grempa.Dynamic

import Fun
-- We also need the token datatype in scope or Template Haskell will whine
import Lex

parseIt = $(mkStaticParser lang [|lang|])
--parseIt = mkDynamicParser constrWrapper lang

main = print . lexAndParse =<< readFile . head =<< getArgs

lexAndParse = parseIt . lexit
