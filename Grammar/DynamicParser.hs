module DynamicParser where

import Data.Data
import Data.Dynamic
import Data.Maybe

import SLR
import Table
import Token
import qualified Typed as T
import Untyped

runSLRG :: (Token s', Token s, Typeable a) => (s -> s')
        -> T.GRId s a -> [s] -> T.Grammar s (ReductionTree s', ProdFuns)
runSLRG c g inp = do
    g' <- T.augment g
    let (unt, funs) = unType c g'
        (at,gt,st)  = slr unt
        res         = driver (toFun actionError at, toFun gotoError gt, st) $ map c inp
    return (res, funs)

runSLRGRes :: (Token s, Token s', Typeable a)
       => (s -> s') -> (s' -> s) -> T.GRId s a -> [s] -> T.Grammar s a
runSLRGRes c unc g inp = do
    (res, funs) <- runSLRG c g inp
    return $ fromJust $ fromDynamic $ rtToTyped unc funs res

runSLR  :: (Token s, Typeable a) => T.GRId s a -> [s] -> T.Grammar s a
runSLR  = runSLRGRes id id

runSLRC :: (Token s, Typeable a) => T.GRId s a -> [s] -> T.Grammar s a
runSLRC = runSLRGRes CTok unCTok

actionError, gotoError :: String
actionError = "ActionTable"
gotoError   = "GotoTable"

