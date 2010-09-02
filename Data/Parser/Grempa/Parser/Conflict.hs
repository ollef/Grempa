module Data.Parser.Grempa.Parser.Conflict where

import qualified Control.Arrow as A
import Data.Function
import Data.List

import Data.Parser.Grempa.Grammar.Token
import Data.Parser.Grempa.Parser.Table

type Conflict s = (StateI, [(Tok s, Action s)])

-- | Check an action table to see if there are any conflicts.
--   If there is a conflict, try to resolve it.
actionTableConflicts :: ActionTable s -> (ActionTable s, [Conflict s])
actionTableConflicts tab = (tab, conflicts)
  where
    conflicts = filter (not . null . snd)
        [(st, filter ((>=2) . length)
                  $ groupBy ((==) `on` fst)
                  $ sort acts)
         | (st, (acts, _)) <- tab]
