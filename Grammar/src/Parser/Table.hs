{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Parser.Table where

import Data.Dynamic
import Data.List
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Language.Haskell.TH.Lift

import Grammar.Token

import Debug.Trace

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

type ActionTable s = Map StateI (Map (Tok s) (Action s), Action s)
type GotoTable   s = Map StateI (Map RuleI StateI)

type ActionFun s   = StateI -> Tok s -> Action s
type GotoFun   s   = StateI -> RuleI -> StateI

actToFun :: Ord s => ActionTable s -> ActionFun s
actToFun table st t = maybe def id $ M.lookup t stateTable
  where
    (stateTable, def) = maybe (error "Invalid parsing state") id $ M.lookup st table

gotoToFun :: GotoTable s -> GotoFun s
gotoToFun table st rule = maybe (error "Goto table") id $ M.lookup rule table'
  where
    table' = maybe (error "Invalid parsing state") id $ M.lookup st table

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

type ProdFuns = Map (RuleI, ProdI) DynFun

rtToTyped :: Token s => (s' -> s) -> ProdFuns -> ReductionTree s' -> Dynamic
rtToTyped unc _    (RTTerm s)   = toDyn (unc s)
rtToTyped unc funs (RTReduce r p tree) = applDynFun fun l
  where
    l           = map (rtToTyped unc funs) tree
    fun         = fromJust $ M.lookup (r, p) funs

driver :: Token s => (ActionFun s, GotoFun s, StateI) -> [s] -> ReductionTree s
driver (actionf, gotof, start) input =
    driver' [start] (map Tok input ++ [RightEnd]) [] []
  where
    driver' stack@(s:_) (a:rest) rt ests = --trace (show stack ++ "," ++ show (a:rest) ) $
      case actionf s a of
          Shift t -> driver' (t : stack) rest (RTTerm (unTok a) : rt) []
          Reduce rule prod len es -> driver' (got : stack') (a : rest) rt' (es ++ ests)
            where
              stack'@(t:_) = drop len stack
              got          = gotof t rule
              rt' = RTReduce rule prod (reverse $ take len rt) : drop len rt
          Accept -> head rt
          Error es -> error $ show $ nub $ es ++ ests
    driver' _ _ _ _ = error "driver'"
