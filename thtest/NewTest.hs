{-# LANGUAGE GADTs, PackageImports, DoRec #-}
module NewTest where

import "monads-fd" Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S


type Rule s = [Prod s]
type Prod s = [Atom s]

data Atom s
    = ASymbol s
    | ARule (RId s)
  deriving (Show, Eq, Ord)

data RId s = RId {rId :: Int, rIdRule :: Rule s}

instance Show s => Show (RId s) where
    show (RId i _) = show i
instance Eq (RId s) where
    RId i _ == RId j _ = i == j
instance Ord (RId s) where
    RId i _ `compare` RId j _ = i `compare` j

data Item s = Item
    { itRId  :: RId s
    , itProd :: Int
    , itPos  :: Int
    }
  deriving (Eq, Ord)

getItProd :: Item s -> Prod s
getItProd it = rIdRule (itRId it) !! itProd it

instance Show s => Show (Item s) where
    show (Item (RId i r) prod pos) =
        concat $ take pos as ++ "*" : drop pos as
      where
        production = r !! prod
        as = map show production

data GrammarState s = GrammarState
    { ids   :: [Int]
    , rules :: [RId s]
    }

type Grammar s a = State (GrammarState s) a

addRule :: Rule s -> Grammar s (RId s)
addRule rule = do
    st <- get
    let i : is = ids st
        rid    = RId i rule
    put st {ids = is, rules = rid : rules st}
    return $ rid

evalGrammar :: Grammar s a -> a
evalGrammar = flip evalState def
  where
    def = GrammarState
        { ids   = [0..]
        , rules = []
        }
showGrammar :: Show s => Grammar s (RId s) -> String
showGrammar g = show r
  where
    RId _ r = evalGrammar g

sym    = ASymbol
rule   = ARule

e = do
    rec
      e  <- addRule [[rule e, sym '+', rule t]
                    ,[rule t]]
      t  <- addRule [[rule t, sym '*', rule f]
                    ,[rule f]]
      f  <- addRule [[sym '(', rule e, sym ')']
                    ,[sym 'x']]
    return e

test = do
    rec
      e <- addRule [[sym '1', rule e], [sym '1']]
    return e
