-- | Generate arbitrary input strings for a grammar and see that it is
--   able to parse them.
module Data.Parser.Grempa.Test(prop_parser) where

import Control.Applicative
import qualified Control.Arrow as A
import Data.Dynamic
import Data.List
import Data.Maybe
import Test.QuickCheck

import qualified Data.Parser.Grempa.Grammar.Typed as T
import Data.Parser.Grempa.Grammar.Untyped
import Data.Parser.Grempa.Parser.Table
import Data.Parser.Grempa.Parser.Result

arb :: Typeable s => ProdFunFun -> RId s -> Int -> Gen ([s], Dynamic)
arb fun rid n = arbR n fun (rIdRule rid, rId rid)

arbR :: Typeable s => Int -> ProdFunFun -> (Rule s, RuleI) -> Gen ([s], Dynamic)
arbR n fun (prods, r) = do
    let (recs, nonRecs) = partition (isRec . fst3) $ index prods
        recsf           = map (tup recf) recs
        nonRecsf        = map (tup $ 10 * recf + 1) nonRecs
        freqs           = map (A.second $ arbP (n - 1) fun) $ recsf ++ nonRecsf
        minn            = if null nonRecs then 1 else 0
        recf            = max n minn
    frequency freqs
  where
    index xs     = zip3 xs [0..] $ repeat r
    fst3 (a,_,_) = a
    tup a b      = (a, b)

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

-- | QuickCheck property for seeing if a parser can parse everything produced
--   by a grammar and get the expected result.
--
--   There are cases where the property will fail even though the parser is
--   correct. That can happen when there is an 'epsilon' production that makes
--   it valid to make the result tree nest one more level without eating any of
--   the input. The parsers generated will not do this, but the random input
--   generator currently will (this is a bug).
--   An example of this is the following:
--
-- > data Expr = ... | EApp Expr [Expr]
-- > grammar = ...
-- >     expr <- rule [...
-- >                  , EApp <@> expr <#> exprs
-- >                  ]
-- >     exprs <- several expr
--
--   Here, the random generator may produce @EApp expr []@ for some @expr@,
--   as the rule 'several' @expr@ matches 0 or more @expr@s.
--   which will have the same input token string as just @expr@ which is what
--   the parser will parse, so the expected result and the parsed result will
--   differ.
prop_parser :: (Show a, Show s, Eq a, Typeable a, Typeable s)
            => Parser s a    -- ^ Input parser
            -> T.Grammar s a -- ^ The grammar used to generate the parser
            -> Property
prop_parser parser grammar =
    let (rid, funs) = unType id $ T.evalGrammar grammar
    in forAll (A.second (fromJust . fromDynamic)
                  <$> sized (arb (prodFunToFun funs) rid))
           (parseCorrect parser)

parseCorrect :: (Eq a) => Parser s a -> ([s], a) -> Bool
parseCorrect parser (inp, res) = case parser inp of
    Right parseres -> parseres == res
    Left _         -> False
