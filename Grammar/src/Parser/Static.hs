{-# LANGUAGE TemplateHaskell #-}
module Parser.Static where

import Control.Applicative
import Data.Dynamic
import Data.Maybe
import qualified Data.Map as M
import Data.Data
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Parser.LALR
import Parser.Table
import qualified Grammar.Typed as T
import Grammar.Token
import Grammar.Untyped

class ToPat a where
    toPat :: a -> PatQ

instance ToPat Char where
    toPat = litP . charL

instance ToPat Int where
    toPat = litP . integerL . fromIntegral

instance (ToPat a, ToPat b) => ToPat (a, b) where
    toPat (x, y) = tupP [toPat x, toPat y]

instance ToPat a => ToPat (Tok a) where
    toPat (Tok x)   = conP 'Tok [toPat x]
    toPat RightEnd  = conP 'RightEnd []

instance ToPat a => ToPat [a] where
    toPat = listP . map toPat

toConstrPat :: (Token s, Lift s) => s -> PatQ
toConstrPat x = do
    let name = mkName $ show $ toConstr x
    info <-reify name
    case info of
        DataConI n t _ _ -> conP n $ replicate (numArgs t) wildP
        _                -> undefined

numArgs :: Type -> Int
numArgs (AppT _ t2) = 1 + numArgs t2
numArgs _           = 0

mkActFun :: (ToPat s, Data s, Lift s) => ActionTable s -> ExpQ
mkActFun tab = do
    st  <- newName "st"
    tok <- newName "tok"
    lamE [varP st, varP tok]
        $ caseE (varE st)
            $ map (mkMatch tok) tab
                ++ [match wildP (normalB [|error "Invalid parsing state"|]) []]
  where
    mkMatch tok (st, (tokTab, def)) =
        match (toPat st) (normalB
            ( caseE (varE tok)
                $ map mkMatch' (M.toList tokTab)
                    ++ [match wildP (normalB [|def|]) []]
            )) []
    mkMatch' (v, res) = match (toPat v) (normalB [|res|]) []

mkGotoFun :: GotoTable s -> ExpQ
mkGotoFun tab = do
    st <- newName "st"
    r  <- newName "r"
    lamE [varP st, varP r]
        $ caseE (tupE [varE st, varE r])
            $ map mkMatch tab
            ++ [match wildP (normalB [|error "Invalid parsing state"|]) []]
  where
    mkMatch (k, v) =
        match (toPat k) (normalB [|v|]) []

runSLRGTH :: (Typeable a, ToPat s, Token s, Lift s)
          => T.GRId s a -> (ExpQ, ProdFunTable)
runSLRGTH g = T.evalGrammar $ do
    g' <- T.augment g
    let (unt, funs) = unType id g'
        (at,gt,st)  = lalr unt
        res         = [|driver ($(mkActFun at), $(mkGotoFun gt), st)|]
    return (res, funs)

mkStaticParser :: (Typeable a, ToPat s, Token s, Lift s)
               => T.GRId s a -> ExpQ -> ExpQ
mkStaticParser g gn = do
    drive  <- newName "driver"
    let driverf = funD drive
                  [clause [] (normalB [| \inp -> ($res inp) |]) []]
    letE [driverf] [| thDriver $gn $(varE drive) |]
  where (res, _) = runSLRGTH g

thDriver :: (Token s, Typeable a) => T.GRId s a -> ([s] -> ReductionTree s) -> [s] -> a
thDriver g f inp = fromJust $ fromDynamic $ rtToTyped id (prodFunToFun funs) (f inp)
  where
    funs = T.evalGrammar (snd <$> unType id <$> T.augment g)
