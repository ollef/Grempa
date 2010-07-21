module Parser.Dynamic where

import Data.Data
import Data.Dynamic
import Data.Maybe

import Parser.SLR
import Parser.LALR
import Parser.Table
import Grammar.Token
import qualified Grammar.Typed as T
import Grammar.Untyped

actToFun :: Ord s => ActionTable s -> ActionFun s
actToFun table st t = maybe def id $ M.lookup t stateTable
  where
    a                 = mapToArr table
    (stateTable, def) = a ! st

gotoToFun :: GotoTable s -> GotoFun s
gotoToFun table st rule = a ! st ! rule
  where
    a      = mapToArr $ M.map mapToArr table


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
