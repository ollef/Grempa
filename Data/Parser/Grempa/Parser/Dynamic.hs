module Data.Parser.Grempa.Parser.Dynamic
    ( mkDynamicParser
    , constrWrapper
    , idWrapper
    ) where

import Data.Array
import Data.Data
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe

import Data.Parser.Grempa.Aux.Aux
import Data.Parser.Grempa.Parser.Driver
import Data.Parser.Grempa.Parser.Error
import Data.Parser.Grempa.Parser.LALR
import Data.Parser.Grempa.Parser.Table
import Data.Parser.Grempa.Grammar.Token
import qualified Data.Parser.Grempa.Grammar.Typed as T
import Data.Parser.Grempa.Grammar.Untyped

actToFun :: Ord s => ActionTable s -> ActionFun s
actToFun table st t = fromMaybe def $ M.lookup t stateTable
  where
    a                 = listToArr table
    (stateTable, def) = a ! st

gotoToFun :: GotoTable s -> GotoFun s
gotoToFun table st rule = a ! (st, rule)
  where
    a      = listToArr table

dynamicRT :: (Token s', Token s, Typeable a) => (s -> s')
        -> T.GRId s a -> [s]
        -> T.Grammar s (ParseResult s' (ReductionTree s'), ProdFunTable)
dynamicRT c g inp = do
    g' <- T.augment g
    let (unt, funs) = unType c g'
        (at,gt,st)  = lalr unt
        res         = driver (actToFun at, gotoToFun gt, st) $ map c inp
    return (res, funs)

mkDynamicParser :: (Token s, Token s', Typeable a)
       => (s -> s', s' -> s) -> T.GRId s a -> Parser s a
mkDynamicParser (c, unc) g inp =
    let (res, funs) = T.evalGrammar $ dynamicRT c g inp
     in resultDriver unc funs g res

constrWrapper :: (s -> CTok s, CTok s -> s)
constrWrapper = (CTok, unCTok)
idWrapper     :: (s -> s, s -> s)
idWrapper     = (id,   id)
