-- | Check parse tables for conflicts and resolve them.
module Data.Parser.Grempa.Parser.Conflict
    ( Conflict
    , conflicts
    , showConflict
    ) where

import qualified Control.Arrow as A
import Data.Function
import Data.List

import Data.Parser.Grempa.Grammar.Token
import Data.Parser.Grempa.Parser.Table

type Conflict t = (StateI, [[(Tok t, Action t)]])

-- | Check an action table to see if there are any conflicts.
--   If there is a conflict, try to resolve it.
conflicts :: Ord t
          => ActionTable t
          -- ^ Input table with potential conflicts
          -> (ActionTable t, [Conflict t])
          -- ^ Corrected action table, and its conflicts
conflicts tab = (tab', cs)
  where
    cs = filter (not . null . snd)
        [(st, filter ((>=2) . length)
                  $ groupBy ((==) `on` fst)
                  $ nub
                  $ sort acts)
         | (st, (acts, _)) <- tab]
    tab' = map (A.second (A.first (nub . sort))) tab

-- | Show a conflict in a readable way
showConflict :: Show t => Conflict t -> String
showConflict (st, confs)
    =  "Warning: Conflicts in action table (state " ++ show st
    ++ "), between " ++ intercalate " and " (map go confs)
  where
    go cs = "[" ++ intercalate "," (map go' cs) ++ "]"
    go' (t, a) = "On token " ++ tokToString t ++ " " ++ showAction a
    showAction (Shift s)       = "shift state " ++ show s
    showAction (Reduce r p _ _) = "reduce (rule " ++ show r ++ ", production " ++ show p ++ ")"
    showAction Accept           = "accept"
    showAction (Error {})       = "error"
