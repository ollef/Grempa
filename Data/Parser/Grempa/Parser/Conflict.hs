-- | Check parse tables for conflicts and resolve them.
module Data.Parser.Grempa.Parser.Conflict
    ( actionConflicts
    , gotoConflicts
    ) where

import Data.Function
import Data.List

import Data.Parser.Grempa.Grammar.Token
import Data.Parser.Grempa.Parser.Table

type ActionConflict t = (StateI, [[(Tok t, Action t)]])
type GotoConflict     = [((StateI, RuleI), StateI)]

-- | Check an action table to see if there are any conflicts.
--   If there is a conflict, try to resolve it.
actionConflicts :: Ord t
                -- | Input table with potential conflicts
                => ActionTable t
                -- | Fixed action table, and its conflicts
                -> (ActionTable t, [ActionConflict t])
actionConflicts tab = (tab, conflicts)
  where
    conflicts = filter (not . null . snd)
        [(st, filter ((>=2) . length)
                  $ groupBy ((==) `on` fst)
                  $ nub
                  $ sort acts)
         | (st, (acts, _)) <- tab]

-- | Check a goto table to see if there are any conflicts.
--   If there is a conflict, try to resolve it.
gotoConflicts
              -- | Input table with potential conflicts
              :: GotoTable t
              -- | Fixed goto table, and its conflicts
              -> (GotoTable t, [GotoConflict])
gotoConflicts tab = (tab, conflicts)
  where
    conflicts = filter ((>=2) . length)
              $ groupBy ((==) `on` fst)
              $ sort tab
