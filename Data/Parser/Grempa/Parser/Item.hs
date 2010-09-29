{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Parser.Grempa.Parser.Item
    ( It(..), getItProd, isKernelIt
    , kernel
    , nextSymbol
    , nextItPos
    , Gen, GenData(..), runGen, gen
    , askItemSet
    , precomputeGotos
    , askGoto
    ) where

import Control.Applicative
import Control.Monad.Reader
import Data.List
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as S

import Data.Parser.Grempa.Aux.Aux
import Data.Parser.Grempa.Grammar.Untyped
import Data.Parser.Grempa.Parser.Table
import Data.Parser.Grempa.Grammar.Token

class (Eq (i s), Ord (i s), Show (i s), Token s) => It i s where
    itRId     :: i s -> RId s
    itProd    :: i s -> ProdI
    getItPos  :: i s -> Int
    setItPos  :: i s -> Int -> i s
    closure   :: Set (i s) -> Set (i s)
    startItem :: RId s -> i s

getItProd :: It i s => i s -> Prod s
getItProd i = rIdRule (itRId i) !! itProd i

isKernelIt :: It i s => RId s -> i s -> Bool
isKernelIt st it = pos > 0 || (itRId it == st && pos == 0)
  where pos = getItPos it

-- | Get the kernel of an item set
kernel :: It i s => RId s -> Set (i s) -> Set (i s)
kernel st = S.filter $ isKernelIt st

-- | Return the symbol to the right of the "dot" in the item
nextSymbol :: It i s => i s -> Tok (Symbol s)
nextSymbol i
    | pos < length prod = Tok $ prod !! pos
    | otherwise         = EOF
  where prod = getItProd i
        pos  = getItPos i

-- | Determine the state transitions in the parsing
goto :: (It i s, Token s) => Set (i s) -> Symbol s -> Set (i s)
goto is s = closure $ setFromJust $ S.map (nextTest s) is
  where
    nextTest x i
      | nextSymbol i == Tok x = Just $ nextItPos i
      | otherwise             = Nothing

nextItPos :: It i s => i s -> i s
nextItPos i = setItPos i $ getItPos i + 1

-- | The sets of items for a grammar
itemSets :: (It i s, Token s) => RId s -> [RId s] -> Set (Set (i s))
itemSets rid rids = S.delete S.empty $ recTraverseG itemSets' c1
  where
    c1            = S.singleton $ closure $ S.singleton $ startItem rid
    symbols       = terminals rids ++ nonTerminals rids
    itemSets' c   = (c `S.union` gs, gs)
      where gs    = S.fromList [goto i x | i <- S.toList c, x <- symbols]

-- | Data environment for parser generation
data GenData i s = GenData
  { gItemSets     :: [(Set (i s), StateI)]
  , gItemSetIndex :: Map (Set (i s)) StateI
  , gRules        :: [RId s]
  , gTerminals    :: [Symbol s]
  , gNonTerminals :: [Symbol s]
  , gSymbols      :: [Symbol s]
  , gStartState   :: Int
  , gStartRule    :: RId s
  , gGotos        :: Map (StateI, Symbol s) StateI
  } deriving Show

type Gen i s = Reader (GenData i s)
runGen :: Gen i s a -> GenData i s -> a
runGen = runReader

-- | Create an initial parser generator data structure
gen :: (It i s, Token s) => RId s -> GenData i s
gen g = GenData
    { gItemSets     = items
    , gItemSetIndex = itemIx
    , gRules        = ruless
    , gTerminals    = terms
    , gNonTerminals = nonTerms
    , gSymbols      = syms
    , gStartState   = snd $ fromMaybe (error "gen: maybe")
                    $ find (S.member (startItem g) . fst) items
    , gStartRule    = g
    , gGotos        = precomputeGotos items itemIx syms
    }
  where items    = zip (S.toList $ itemSets g ruless) [0..]
        itemIx   = M.fromList items
        ruless   = rules g
        terms    = terminals ruless
        nonTerms = nonTerminals ruless
        syms     = terms ++ nonTerms

-- | Calculate the goto function for all inputs and put it in a map
precomputeGotos :: (It i s, Token s)
                => [(Set (i s), StateI)] -> Map (Set (i s)) StateI -> [Symbol s]
                -> Map (StateI, Symbol s) StateI
precomputeGotos iss isi syms = M.fromList
        [((ii, sym), st) | (is, ii) <- iss
                         , sym      <- syms
                         , Just st <- [findState $ goto is sym]]
  where
    findState = lookupItemSet iss isi


lookupItemSet :: (It i s, Token s)
              => [(Set (i s), StateI)] -> Map (Set (i s)) StateI
              -> Set (i s)
              -> Maybe StateI
lookupItemSet iss isi x
    | S.null x  = Nothing
    | otherwise = case M.lookup x isi of
        Nothing -> snd <$> listToMaybe (filter (S.isSubsetOf x . fst) iss)
        y       -> y

-- | Get what item set index an item set corresponds to
askItemSet :: (It i s, Token s) => Set (i s) -> Gen i s (Maybe StateI)
askItemSet x = do
    iss <- asks gItemSets
    isi <- asks gItemSetIndex
    return $ lookupItemSet iss isi x

-- | Lookup a precomputed goto value
askGoto :: (It i s, Token s) => StateI -> Symbol s -> Gen i s (Maybe StateI)
askGoto st sym = M.lookup (st, sym) <$> asks gGotos
