-- | Generate arbitrary input strings for a grammar and see that it is
--   able to parse them.
module Data.Parser.Grempa.Test.Arb(prop_parser) where

import Control.Applicative
import qualified Control.Arrow as A
import Data.Dynamic
import Data.Maybe
import Test.QuickCheck

import qualified Data.Parser.Grempa.Grammar.Typed as T
import Data.Parser.Grempa.Grammar.Untyped
import Data.Parser.Grempa.Parser.Table
import Data.Parser.Grempa.Parser.Result

arb :: Typeable s => ProdFunFun -> RId s -> Int -> Gen ([s], Dynamic)
arb fun rid n = arbR n fun (rIdRule rid, rId rid)

arbR :: Typeable s => Int -> ProdFunFun -> (Rule s, RuleI) -> Gen ([s], Dynamic)
arbR n fun (prods, r)
  | n > 0     = arby $ index prods
  | otherwise = do
    let nonRecs = filter (not . isRec . fst3) $ index prods
    if not $ null nonRecs
        then arby nonRecs
        else arby $ index prods
  where
    index xs     = zip3 xs [0..] $ repeat r
    fst3 (a,_,_) = a
    arby xs      = (arbP n fun =<< elements xs) -- `suchThat` (not . null . fst)

arbP :: Typeable s => Int -> ProdFunFun -> (Prod s, RuleI, ProdI) -> Gen ([s], Dynamic)
arbP n fun (prod, p, r) = do
    (syms, dyns) <- A.first concat
                        <$> unzip
                        <$> mapM (arbS n fun) prod
    return (syms, applDynFun (fun r p) dyns)

arbS :: Typeable s => Int -> ProdFunFun -> Symbol s -> Gen ([s], Dynamic)
arbS _ _   (STerm s)   = return ([s], toDyn s)
arbS n fun (SRule rid) = arb fun rid (n - 1)

isRec :: Prod s -> Bool
isRec = not . null . filter isRule
  where
    isRule (SRule {}) = True
    isRule _          = False

prop_parser :: (Show a, Show s, Eq a, Typeable a, Typeable s) => Parser s a -> T.Grammar s a -> Property
prop_parser parser grammar =
    let (rid, funs) = unType id $ T.evalGrammar grammar
    in forAll (A.second (fromJust . fromDynamic)
                  <$> sized (arb (prodFunToFun funs) rid))
           (parseCorrect parser)

parseCorrect :: (Eq a) => Parser s a -> ([s], a) -> Bool
parseCorrect parser (inp, res) = case parser inp of
    Right parseres -> parseres == res
    Left _         -> False
