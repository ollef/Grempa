{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
-- | Make parsers at compile time using Template Haskell
module Data.Parser.Grempa.Parser.Static
    ( mkStaticParser
    , ToPat(..)
    , toConstrPat
    ) where

import Control.Applicative
import Control.Monad
import Data.Dynamic
import Data.Data
import Language.Haskell.TH hiding (unType)
import Language.Haskell.TH.Syntax hiding (unType)

import Data.Parser.Grempa.Parser.Conflict
import Data.Parser.Grempa.Parser.Driver
import Data.Parser.Grempa.Parser.LALR
import Data.Parser.Grempa.Parser.Table
import qualified Data.Parser.Grempa.Grammar.Typed as T
import Data.Parser.Grempa.Grammar.Token
import Data.Parser.Grempa.Grammar.Untyped
import Data.Parser.Grempa.Parser.Result -- For Haddock!

-- | Make a function with a case expression from an action table
mkActFun :: (ToPat t, Data t, Lift t) => ActionTable t -> ExpQ
mkActFun tab = do
    st  <- newName "st"
    tok <- newName "tok"
    lamE [varP st, varP tok]
        $ caseE (varE st)
            $ map (mkMatch tok) tab
                ++ [match wildP (normalB [|Error []|]) []]
  where
    mkMatch tok (st, (tokTab, def)) =
        match (toPat st) (normalB
            ( caseE (varE tok)
                $ map mkMatch' tokTab
                    ++ [match wildP (normalB [|def|]) []]
            )) []
    mkMatch' (v, res) = match (toPat v) (normalB [|res|]) []

-- | Make a function with a case expression from a goto table
mkGotoFun :: GotoTable t -> ExpQ
mkGotoFun tab = do
    st <- newName "st"
    r  <- newName "r"
    lamE [varP st, varP r]
        $ caseE (tupE [varE st, varE r])
            $ map mkMatch tab
            ++ [match wildP (normalB [|-1 :: Int|]) []] -- Hacky (unknown goto is -1)
  where
    mkMatch (k, v) =
        match (toPat k) (normalB [|v|]) []

-- | Make a function returning the reduction tree from a grammar
staticRT :: (Typeable a, ToPat t, Token t, Lift t)
          => T.Grammar t a -> ExpQ
staticRT g = do
    let (res, confls) = T.evalGrammar $ do
        g' <- T.augment g
        let (unt, _)    = unType id g'
            (at,gt,st)  = lalr unt
            (at', ac)   = conflicts at
            driv        = [|driver ($(mkActFun at'), $(mkGotoFun gt), st)|]
        return (driv, ac)
    forM_ confls $ reportWarning . showConflict
    res

-- | Make a static parser from a grammar.
--
--   Example usage:
--
-- > g :: Grammar s a
-- > gparser = $(mkStaticParser g [|g|])
--
--   Note that @gparser@ must be in a different module than @g@, due to
--   Template Haskell restrictions.
--   The token type of the grammar must also be an instance of 'ToPat', and the
--   result type an instance of 'Typeable' (the GHC extension
--   DeriveDataTypeable may be useful for this).
--
--   If there are conflicts in the parsing tables, they will be displayed
--   as warnings when compiling the parser.
mkStaticParser :: (Typeable a, ToPat t, Token t, Lift t)
               => T.Grammar t a -- ^ The grammar
               -> ExpQ          -- ^ The Template Haskell representation of the
                                --   grammar
               -> ExpQ          -- ^ The representation of a parser of type 
                                --   'Parser' @t a@
mkStaticParser g gn = do
    drive  <- newName "driver"
    inp    <- newName "inp"
    let driverf = funD drive
                  [clause [varP inp] (normalB [| $(staticRT g) $(varE inp) |]) []]
    letE [driverf] [| resultDriver id $funs $gn . $(varE drive) |]
  where
    funs = [| T.evalGrammar $ snd <$> unType id <$> T.augment $gn |]

-- | Make a Template Haskell pattern from a value.
--   This is used to create a case expression from a parsing table when using
--   'mkStaticParser', and it is thus required that the token type that the
--   parser is to operate on is an instance of this class.
--
--   The parser will behave differently depending on how its 'ToPat' instance
--   works. If only comparing constructors ('toConstrPat'), it will regard
--   @Just 1@ as the same compared to @Just 2@.
--
--   'toConstrPat' and "Language.Haskell.TH" can help in creating an instance.
class ToPat a where
    toPat :: a -> PatQ

instance ToPat Char where
    toPat = litP . charL

instance ToPat Int where
    toPat = litP . integerL . fromIntegral

instance (ToPat a, ToPat b) => ToPat (a, b) where
    toPat (x, y) = tupP [toPat x, toPat y]

instance ToPat a => ToPat (Tok a) where
    toPat (Tok x) = conP 'Tok [toPat x]
    toPat EOF     = conP 'EOF []

instance ToPat a => ToPat [a] where
    toPat = listP . map toPat

-- | Automatically create a 'ToPat' instance which only compares the constructor
--   of the token type. For example, the pattern yielded from using this on the
--   value @Just 3@ is the pattern @Just _@.
--
--   Example usage:
--
-- > instance ToPat TokenType where
-- >     toPat = toConstrPat
toConstrPat :: (Token t, Lift t) => t -> PatQ
toConstrPat tok = do
    Just name <- lookupValueName $ tyconModule (dataTypeName $ dataTypeOf tok)
                                   ++ show (toConstr tok)
    info <-reify name
    case info of
        DataConI n t _ _ -> conP n $ replicate (numArgs t) wildP
        x                -> error $ "toConstrPat got " ++ show x
  where
    numArgs (AppT _ t2) = 1 + numArgs t2
    numArgs _           = 0
