module Data.Parser.Grempa.Test.Arb where

import Control.Applicative
import qualified Data.Parser.Grempa.Grammar.Typed as T
import Data.Parser.Grempa.Grammar.Untyped

import Test.QuickCheck

arb :: Int -> RId s -> Gen [s]
arb n rid = arbR n (rIdRule rid)

arbR :: Int -> Rule s -> Gen [s]
arbR n prods
  | n > 0 = arbP (n - 1) =<< elements prods
  | otherwise = do
    let nonRecs = filter (not . isRec) prods
    if not $ null nonRecs
        then arbP n =<< elements nonRecs
        else arbP n =<< elements prods

arbP :: Int -> Prod s -> Gen [s]
arbP n p = concat <$> mapM (arbS n) p

arbS :: Int -> Symbol s -> Gen [s]
arbS n (STerm s) = return [s]
arbS n (SRule rid) = arb n rid

isRec :: Prod s -> Bool
isRec = not . null . filter isRule
  where
    isRule (SRule _) = True
    isRule _         = False


