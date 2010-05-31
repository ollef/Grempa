{-# LANGUAGE GADTs, DoRec, PackageImports #-}
module Untyped where

import Control.Applicative
import "monads-fd" Control.Monad.State
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

unType :: T.RId s a -> RId s
unType = flip evalState M.empty . unTypeR
  where
    unTypeR :: T.RId s a -> State (Map Int (RId s)) (RId s)
    unTypeR (T.RId i r) = do
        rids <- get
        case M.lookup i rids of
            Just r  -> return r
            Nothing -> do
              rec
                put $ M.insert i res rids
                res <- RId i <$> mapM unTypeP r
              return res
    unTypeP :: T.Prod s a -> State (Map Int (RId s)) (Prod s)
    unTypeP p = case p of
        T.PSeq s ps -> liftM2 (:) (unTypeS s) (unTypeP ps)
        T.PEnd _    -> return []
    unTypeS :: T.Symbol s a -> State (Map Int (RId s)) (Symbol s)
    unTypeS s = case s of
        T.STerm s -> return $ STerm s
        T.SRule r -> SRule <$> unTypeR r
