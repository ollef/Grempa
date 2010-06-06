{-# LANGUAGE TemplateHaskell #-}
module Test2 where

import Test
import TableFuns

$(runSLRGResTH e)
