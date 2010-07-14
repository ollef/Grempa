{-# LANGUAGE TemplateHaskell #-}
module Table where

import Data.Dynamic
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Language.Haskell.TH.Lift

import Token

import Debug.Trace

-- | Data type used in the action table to determine the next
--   parsing action depending on the input and current state
data Action = Shift  Int
            | Reduce Int (Int, Int)
            | Accept
  deriving (Show)

$(deriveLift ''Action)

type ActionTable s = Map (Int, Tok s) Action
type GotoTable   s = Map (Int, Int)   Int

type ActionFun s   = (Int, Tok s) -> Action
type GotoFun   s   = (Int, Int)   -> Int

toFun :: (Show a, Ord a) => String -> Map a b -> a -> b
toFun err m x = maybe (error $ err ++ " " ++ show x) id $ M.lookup x m

-- | Data type for reduction trees output by the driver
data ReductionTree s
    = RTReduce Int Int [ReductionTree s]
    | RTTerm s
  deriving Show

data DynFun = DynFun Dynamic [Bool]

applDynFun :: DynFun -> [Dynamic] -> Dynamic
applDynFun (DynFun f (b:bs)) (a:as)
    | b         = applDynFun (DynFun (dynApp f a) bs) as
    | otherwise = applDynFun (DynFun f bs) as
applDynFun (DynFun f _) _ = f

type ProdFuns = Map (Int, Int) DynFun

rtToTyped :: Token s => (s' -> s) -> ProdFuns -> ReductionTree s' -> Dynamic
rtToTyped unc _    (RTTerm s)   = toDyn (unc s)
rtToTyped unc funs (RTReduce r p tree) = applDynFun fun l
  where
    l           = map (rtToTyped unc funs) tree
    fun         = fromJust $ M.lookup (r, p) funs

driver :: Token s => (ActionFun s, GotoFun s, Int) -> [s] -> ReductionTree s
driver (actionf, gotof, start) input =
    driver' [start] (map Tok input ++ [RightEnd]) []
  where
    driver' stack@(s:_) (a:rest) rt = trace (show stack ++ "," ++ show (a:rest) ) $ 
      case actionf (s, a) of
        Shift t -> driver' (t : stack) rest (RTTerm (unTok a) : rt)
        Reduce rule (prod, len) -> driver' (got : stack') (a : rest) rt'
          where
            stack'@(t:_) = drop len stack
            got          = gotof (t, rule)
            rt' = RTReduce rule prod (reverse $ take len rt) : drop len rt
        Accept -> head rt
    driver' _ _ _ = error "driver'"
