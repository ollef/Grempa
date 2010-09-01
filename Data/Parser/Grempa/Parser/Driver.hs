module Data.Parser.Grempa.Parser.Driver where

import Control.Applicative
import Data.Dynamic
import Data.List
import Data.Maybe

import Data.Parser.Grempa.Parser.Result
import Data.Parser.Grempa.Parser.Table
import qualified Data.Parser.Grempa.Grammar.Typed as T
import Data.Parser.Grempa.Grammar.Token

-- | Data type for reduction trees output by the driver
data ReductionTree s
    = RTReduce RuleI ProdI [ReductionTree s]
    | RTTerm s
  deriving Show

rtToTyped :: Token s => (s' -> s) -> ProdFunFun -> ReductionTree s' -> Dynamic
rtToTyped unc _    (RTTerm s)   = toDyn (unc s)
rtToTyped unc funs (RTReduce r p tree) = applDynFun fun l
  where
    l           = map (rtToTyped unc funs) tree
    fun         = funs r p

driver :: Token s => (ActionFun s, GotoFun s, StateI) -> [s]
                  -> ParseResult s (ReductionTree s)
driver (actionf, gotof, start) input =
    driver' [start] (map Tok input ++ [EOF]) [] [] (0 :: Integer)
  where
    driver' stack@(s:_) (a:rest) rt ests pos =
      case actionf s a of
          Shift t -> driver' (t : stack) rest (RTTerm (unTok a) : rt) [] (pos + 1)
          Reduce rule prod len es -> driver' (got : stack') (a : rest) rt' (es ++ ests) pos
            where
              stack'@(t:_) = drop len stack
              got          = gotof t rule
              rt' = RTReduce rule prod (reverse $ take len rt) : drop len rt
          Accept -> Right $ head rt
          Error es -> Left $ ParseError (nub $ es ++ ests) pos
    driver' _ _ _ ests pos = Left $ InternalParserError pos

type RTParseResult s = ParseResult s (ReductionTree s)

resultDriver :: (Token s, Typeable a)
             => (s' -> s) -> ProdFunTable -> T.Grammar s a -> RTParseResult s' -> ParseResult s a
resultDriver unc funs _ rt =  fromJust
                          <$> fromDynamic
                          <$> rtToTyped unc (prodFunToFun funs)
                          <$> either (Left . fmap unc) Right rt
