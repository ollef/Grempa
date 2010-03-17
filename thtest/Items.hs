{-# LANGUAGE GADTs #-}
module Items where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set

import Grammar

items :: RId s p -> Grammar s (Set (Int, Int))
items p = items' <$> getRule p
  where
    items' :: Rule s p -> Set (Int, Int)
    items' r = case r of
        Symbol s -> undefined
