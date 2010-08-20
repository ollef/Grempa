module Data.Parser.Grempa.Parser.Dynamic where

import Data.Array
import Data.Data
import Data.Dynamic
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe

import Data.Parser.Grempa.Aux.Aux
import Data.Parser.Grempa.Parser.SLR
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

runSLRG :: (Token s', Token s, Typeable a) => (s -> s')
        -> T.GRId s a -> [s] -> T.Grammar s (ReductionTree s', ProdFunTable)
runSLRG c g inp = do
    g' <- T.augment g
    let (unt, funs) = unType c g'
        (at,gt,st)  = lalr unt
        res         = driver (actToFun at, gotoToFun gt, st) $ map c inp
    return (res, funs)

runSLRGRes :: (Token s, Token s', Typeable a)
       => (s -> s') -> (s' -> s) -> T.GRId s a -> [s] -> T.Grammar s a
runSLRGRes c unc g inp = do
    (res, funs) <- runSLRG c g inp
    return $ fromJust $ fromDynamic $ rtToTyped unc (prodFunToFun funs) res

runSLR  :: (Token s, Typeable a) => T.GRId s a -> [s] -> T.Grammar s a
runSLR  = runSLRGRes id id

runSLRC :: (Token s, Typeable a) => T.GRId s a -> [s] -> T.Grammar s a
runSLRC = runSLRGRes CTok unCTok
