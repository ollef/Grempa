{-# LANGUAGE GADTs, DoRec, PackageImports #-}
module Untyped where

import Control.Arrow
import Control.Applicative
import "monads-fd" Control.Monad.State
import Data.Dynamic
import qualified Data.Map as M
import Data.Map(Map)

import qualified Typed as T

data Item s where
    Item :: { itRId  :: RId s
            , itProd :: Int
            , itPos  :: Int
            } -> Item s
  deriving (Eq, Ord, Show)

getItProd :: Item s -> Prod s
getItProd i = rIdRule (itRId i) !! itProd i

type Rule s = [Prod s]
type Prod s = [Symbol s]

data Symbol s where
    STerm :: s     -> Symbol s
    SRule :: RId s -> Symbol s
  deriving (Eq, Ord, Show)

data RId s = RId {rId :: Int, rIdRule :: Rule s}

instance Show s => Show (RId s) where
    show (RId i _) = show i
instance Eq (RId s) where
    RId i _ == RId j _ = i == j
instance Ord (RId s) where
    RId i _ `compare` RId j _ = i `compare` j

type ProdFuns = Map (Int, Int) Dynamic
-- | Returns an untyped tree representation of a typed grammar
--   together with a mapping from rule and production number to
--   a dynamic containing the construction function of the typed
--   production
unType :: (s -> s') -> T.RId s a -> (RId s', ProdFuns)
unType c = second snd . flip runState (M.empty, M.empty) . unTypeR c
  where
    unTypeR :: (s -> s') -> T.RId s a -> State (Map Int (RId s'), ProdFuns) (RId s')
    unTypeR c (T.RId i r) = do
        (rids, funs) <- get
        case M.lookup i rids of
            Just r  -> return r
            Nothing -> do
                let newfuns = M.fromList
                            $ zip (zip (repeat i) [0..])
                                  (map T.getFun r)
                rec
                  put (M.insert i res rids, funs `M.union` newfuns)
                  res <- RId i <$> mapM (unTypeP c) r
                return res
    unTypeP :: (s -> s') -> T.Prod s a -> State (Map Int (RId s'), ProdFuns) (Prod s')
    unTypeP c p = case p of
        T.PSeq s ps -> liftM2 (:) (unTypeS c s) (unTypeP c ps)
        T.PEnd _    -> return []
    unTypeS :: (s -> s') -> T.Symbol s a -> State (Map Int (RId s'), ProdFuns) (Symbol s')
    unTypeS c s = case s of
        T.STerm s -> return $ STerm (c s)
        T.SRule r -> SRule <$> unTypeR c r
