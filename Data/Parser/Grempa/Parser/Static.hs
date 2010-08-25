{-# LANGUAGE TemplateHaskell #-}
module Data.Parser.Grempa.Parser.Static where

import Control.Applicative
import Data.Dynamic
import qualified Data.Map as M
import Data.Data
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Parser.Grempa.Parser.Driver
import Data.Parser.Grempa.Parser.LALR
import Data.Parser.Grempa.Parser.Driver
import Data.Parser.Grempa.Parser.Table
import qualified Data.Parser.Grempa.Grammar.Typed as T
import Data.Parser.Grempa.Grammar.Token
import Data.Parser.Grempa.Grammar.Untyped

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
toConstrPat t = do
    let name = mkName $ tyconModule (dataTypeName $ dataTypeOf t) ++ "." ++ show (toConstr t)
    info <-reify name
    case info of
        DataConI n t _ _ -> conP n $ replicate (numArgs t) wildP
        x                -> error $ "toConstrPat got " ++ show x

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

staticRT :: (Typeable a, ToPat s, Token s, Lift s)
          => T.GRId s a -> ExpQ
staticRT g = T.evalGrammar $ do
    g' <- T.augment g
    let (unt, _)    = unType id g'
        (at,gt,st)  = lalr unt
        res         = [|driver ($(mkActFun at), $(mkGotoFun gt), st)|]
    return res

mkStaticParser :: (Typeable a, ToPat s, Token s, Lift s)
               => T.GRId s a -> ExpQ -> ExpQ
mkStaticParser g gn = do
    drive  <- newName "driver"
    inp    <- newName "inp"
    let driverf = funD drive
                  [clause [varP inp] (normalB [| $(staticRT g) $(varE inp) |]) []]
    letE [driverf] [| resultDriver id $funs $gn . $(varE drive) |]
  where
    funs = [| T.evalGrammar $ snd <$> unType id <$> T.augment $gn |]
