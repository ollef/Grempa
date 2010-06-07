{-# LANGUAGE TemplateHaskell #-}
module TableFuns where

import Control.Applicative
import Data.Dynamic
import Data.Maybe
import Data.Map(Map, toList)
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

import LR
import Table
import qualified Typed as T
import Token
import Untyped

class ToPat a where
    toPat :: a -> PatQ

instance ToPat Char where
    toPat = litP . charL

instance ToPat Int where
    toPat = litP . integerL . fromIntegral

instance (ToPat a, ToPat b) => ToPat (a, b) where
    toPat (x, y) = tupP [toPat x, toPat y]

instance ToPat a => ToPat (Maybe a) where
    toPat (Just x) = conP 'Just [toPat x]
    toPat Nothing  = conP 'Nothing []

mkFunMap :: (ToPat a, Lift b) => Map a b -> ExpQ
mkFunMap = mkFun . toList

mkFun :: (ToPat a, Lift b) => [(a,b)] -> ExpQ
mkFun ms = do
    x <- newName "x"
    lam1E (varP x)
        $ caseE (varE x)
            $ map mkMatch ms
                ++ [match wildP (normalB [|error "Couldn't match"|]) []]
  where
    mkMatch (a,b) = match (toPat a) (normalB [|b|]) []
    getMatch c = do
        CaseE _ [m] <- c
        return m

runSLRGTH :: (Typeable a)
          => T.GRId Char a -> (ExpQ, ProdFuns)
runSLRGTH g = T.evalGrammar $ do
    g' <- T.augment g
    let (unt, funs) = unType id g'
        (at,gt,st)  = slr unt
        res         = [|driver ($(mkFunMap at), $(mkFunMap gt), st) |]
    return (res, funs)

runSLRGResTH :: (Typeable a)
             => T.GRId Char a -> ExpQ -> String -> Q [Dec]
runSLRGResTH g gn name = do
    driver  <- newName "driver"
    let driverf = funD driver
                  [clause [] (normalB [| \inp -> ($res inp) |]) []]
    res     <- funD (mkName name)
               [clause [] (normalB
                  [| thDriver $gn $(varE driver) |]) [driverf]]

    return [res]
  where (res, _) = runSLRGTH g

thDriver :: (Token s, Typeable a) => T.GRId s a -> ([s] -> ReductionTree s) -> [s] -> a
thDriver g f inp = fromJust $ fromDynamic $ rtToTyped id funs (f inp)
  where
    funs = T.evalGrammar (snd <$> unType id <$> T.augment g)


