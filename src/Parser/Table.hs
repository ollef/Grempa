{-# LANGUAGE TemplateHaskell #-}
module Parser.Table where

import Data.Array
import Data.Dynamic
import Data.List
import Data.Map(Map)
import Language.Haskell.TH.Lift

import Aux
import Grammar.Token

type StateI = Int
type RuleI  = Int
type StackI = Int
type ProdI  = Int

-- | Data type used in the action table to determine the next
--   parsing action depending on the input and current state
data Action s = Shift  StateI
              | Reduce RuleI ProdI StackI [Tok s]
              | Accept
              | Error [Tok s]
  deriving (Eq, Show)

unError :: Action s -> [Tok s]
unError (Error es) = es
unError _          = []

isReduce :: Action s -> Bool
isReduce (Reduce {}) = True
isReduce _          = False

$(deriveLift ''Action)

type ActionTable s = [(StateI, (Map (Tok s) (Action s), Action s))]
type GotoTable   s = [((StateI, RuleI), StateI)]

type ActionFun s   = StateI -> Tok s -> Action s
type GotoFun   s   = StateI -> RuleI -> StateI

type ProdFunTable  = [((RuleI, ProdI), DynFun)]
type ProdFunFun    = RuleI  -> ProdI -> DynFun

prodFunToFun :: ProdFunTable -> ProdFunFun
prodFunToFun table r p = a ! (r, p)
  where a = listToArr table

-- | Data type for reduction trees output by the driver
data ReductionTree s
    = RTReduce RuleI ProdI [ReductionTree s]
    | RTTerm s
  deriving Show

data DynFun = DynFun Dynamic [Bool]

applDynFun :: DynFun -> [Dynamic] -> Dynamic
applDynFun (DynFun f (b:bs)) (a:as)
    | b         = applDynFun (DynFun (dynApp f a) bs) as
    | otherwise = applDynFun (DynFun f bs) as
applDynFun (DynFun f _) _ = f

rtToTyped :: Token s => (s' -> s) -> ProdFunFun -> ReductionTree s' -> Dynamic
rtToTyped unc _    (RTTerm s)   = toDyn (unc s)
rtToTyped unc funs (RTReduce r p tree) = applDynFun fun l
  where
    l           = map (rtToTyped unc funs) tree
    fun         = funs r p

driver :: Token s => (ActionFun s, GotoFun s, StateI) -> [s] -> ReductionTree s
driver (actionf, gotof, start) input =
    driver' [start] (map Tok input ++ [RightEnd]) [] [] 0
  where
    driver' stack@(s:_) (a:rest) rt ests pos = --trace (show stack ++ "," ++ show (a:rest) ) $
      case actionf s a of
          Shift t -> driver' (t : stack) rest (RTTerm (unTok a) : rt) [] (pos + 1)
          Reduce rule prod len es -> driver' (got : stack') (a : rest) rt' (es ++ ests) pos
            where
              stack'@(t:_) = drop len stack
              got          = gotof t rule
              rt' = RTReduce rule prod (reverse $ take len rt) : drop len rt
          Accept -> head rt
          Error es -> error $ "Parse error at " ++ show pos ++ ", expecting " ++ show (nub $ es ++ ests)
    driver' _ _ _ _ pos = error $ "Oh snap! Parse error at " ++ show pos
