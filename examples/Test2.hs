{-# LANGUAGE TemplateHaskell #-}
module Test2 where

import Test.QuickCheck

import Test
import Text.Grempa.Parser.Static

parseE :: String -> E
parseE = $(mkStaticParser e [|e|])

parseList :: String -> String
parseList = $(mkStaticParser list [|list|])

parseEx :: String -> S
parseEx = $(mkStaticParser ex [|ex|])

parseEx454 :: String -> Sx
parseEx454 = $(mkStaticParser ex454 [|ex454|])

parseE1 :: [Sym] -> E1
parseE1 = $(mkStaticParser e1 [|e1|])

prop_e e = parseE (shower e) == e


