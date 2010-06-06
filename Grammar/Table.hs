{-# LANGUAGE TemplateHaskell #-}
module Table where

import Data.Map(Map)
import qualified Data.Map as M
import Language.Haskell.TH
import Language.Haskell.TH.Lift

-- | Data type used in the action table to determine the next
--   parsing action depending on the input and current state
data Action
    = Shift  Int
    | Reduce Int (Int, Int)
    | Accept
  deriving (Show)

$(deriveLift ''Action)

type ActionTable s = Map (Int, Maybe s)   Action
type GotoTable   s = Map (Int, Int) Int

type ActionFun s   = (Int, Maybe s) -> Action
type GotoFun   s   = (Int, Int)     -> Int

toFun :: (Show a, Ord a) => String -> Map a b -> a -> b
toFun err m x = maybe (error $ err ++ " " ++ show x) id $ M.lookup x m

