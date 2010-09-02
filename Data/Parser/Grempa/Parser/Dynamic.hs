{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Parser.Grempa.Parser.Dynamic
    ( mkDynamicParser
    , constrWrapper
    , idWrapper
    ) where

import qualified Control.Arrow as A
import Data.Array
import Data.Data
import Data.Function
import qualified Data.Map as M
import Data.Maybe

import Data.Parser.Grempa.Aux.Aux
import Data.Parser.Grempa.Parser.Driver
import Data.Parser.Grempa.Parser.LALR
import Data.Parser.Grempa.Parser.Result
import Data.Parser.Grempa.Parser.Table
import Data.Parser.Grempa.Grammar.Token
import qualified Data.Parser.Grempa.Grammar.Typed as T
import Data.Parser.Grempa.Grammar.Untyped

-- | Convert an action table to a function (operating on an array)
actToFun :: Ord t => ActionTable t -> ActionFun t
actToFun table st t = fromMaybe def $ M.lookup t stateTable
  where
    a                 = listToArr (M.empty, Error []) table'
    (stateTable, def) = if inRange (bounds a) st
                            then a ! st
                            else (M.empty, Error [])
    table' = map (A.second (A.first M.fromList)) table

-- | Convert an goto table to a function (operating on an array)
gotoToFun :: GotoTable t -> GotoFun t
gotoToFun table st rule = a ! (st, rule)
  where
    a      = listToArr (-1) table

-- | Generate and run a dynamic parser, returning the result reduction tree
dynamicRT :: (Token t', Token t, Typeable a)
        => (t -> t')     -- ^ Token wrapper
        -> T.Grammar t a -- ^ Language grammar
        -> [t]           -- ^ Input token string
        -> T.GrammarState t (ParseResult t' (ReductionTree t'), ProdFunTable)
dynamicRT c g inp = do
    g' <- T.augment g
    let (unt, funs) = unType c g'
        (at,gt,st)  = lalr unt
        res         = driver (actToFun at, gotoToFun gt, st) $ map c inp
    return (res, funs)

-- | Make a parser at runtime given a grammar
mkDynamicParser :: (Token t, Token t', Typeable a)
       => (t -> t', t' -> t) -- ^ Token wrapper and unwrapper
       -> T.Grammar t a      -- ^ Language grammar
       -> Parser t a
mkDynamicParser (c, unc) g inp =
    let (res, funs) = T.evalGrammar $ dynamicRT c g inp
     in resultDriver unc funs g res

-- | Wrapper type for representing tokens only caring about the constructor.
--   The Eq and Ord instances for 'CTok' will only compare the constructors
--   of its arguments.
data CTok a = CTok {unCTok :: a}
  deriving (Show, Data, Typeable)

instance Token a => Eq (CTok a) where
    CTok x == CTok y = ((==) `on` toConstr) x y

instance Token a => Ord (CTok a) where
    CTok x `compare` CTok y = case ((==) `on` toConstr) x y of
        True  -> EQ
        False -> x `compare` y

-- | Wrap the input tokens in the 'CTok' datatype, which has 'Eq' and 'Ord'
--   instances which only look at the constructors of the input values.
--   This is for use as an argument to 'mkDynamicParser'.
--
--   Example, which will evaluate to @True@:
--
-- > CTok (Just 1) == CTok (Just 2)
--
--   This is useful when using a lexer that may give back a list of something
--   like:
--
-- > data Token = Ident String | Number Integer | LParen | RParen | Plus | ...
--
--   If you want to specify a grammar that accepts any @Ident@ and any @Number@
--   and not just specific ones, use 'constrWrapper'.
constrWrapper :: (t -> CTok t, CTok t -> t)
constrWrapper = (CTok, unCTok)

-- | Don't wrap the input tokens.
--   This is for use as an argument to 'mkDynamicParser'.
--   An example usage of 'idWrapper' is if the parser operates directly on
--   'String'.
idWrapper     :: (t -> t, t -> t)
idWrapper     = (id,   id)
