{-# LANGUAGE TemplateHaskell #-}
module Test2 where

import Data.Either
import Test.QuickCheck

import Test
import Data.Parser.Grempa.Static
import Data.Parser.Grempa.Dynamic

parseE :: Parser Char E
parseE = $(mkStaticParser e [|e|])

parseList :: Parser Char String
parseList = $(mkStaticParser list [|list|])

parseEx, parseEx' :: Parser Char S
parseEx = $(mkStaticParser ex [|ex|])
parseEx' = mkDynamicParser idWrapper ex -- $(mkStaticParser ex [|ex|])

parseEx454 :: Parser Char Sx
parseEx454 = $(mkStaticParser ex454 [|ex454|])

parseE1 :: Parser Sym E1
parseE1 = $(mkStaticParser e1 [|e1|])

prop_e e = either undefined id (parseE (shower e)) == e


