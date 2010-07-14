{-# LANGUAGE TemplateHaskell #-}
module Test2 where

import Test
import StaticParser


parseE :: String -> E
parseE = $(mkStaticParser e [|e|])

parseList :: String -> String
parseList = $(mkStaticParser list [|list|])

parseEx :: String -> S
parseEx = $(mkStaticParser ex [|ex|])

parseEx454 :: String -> Sx
parseEx454 = $(mkStaticParser ex454 [|ex454|])
