{-# LANGUAGE TemplateHaskell #-}
module Test2 where

import Test
import StaticParser


parseE :: String -> E
parseE = $(mkStaticParser e [|e|])
