{-# LANGUAGE TemplateHaskell #-}
module Parse where

import Parser.Static

import Lex

parse = $(mkStaticParser lang [|lang|])

lexAndParse = parse . lexit
