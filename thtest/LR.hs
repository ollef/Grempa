{-# LANGUAGE DoRec #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
module LR where

import Control.Applicative
import "monads-fd" Control.Monad.State

import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as S

import Grammar

augment :: Show s => Grammar s (RId s a) -> Grammar s (RId s a)
augment g = do
  rec
    s <- id -= r
    r <- g
  return s

items :: Ord s => RId s a -> Grammar s (Set (Set (Item s)))
items i = do
    cl <- closureR i
    gsyms <- S.toList <$> grammarSymbols
    items' gsyms S.empty $ S.singleton cl
  where
    items' :: Ord s => [Any (Atom s)] -> Set (Set (Item s)) -> Set (Set (Item s))
           -> Grammar s (Set (Set (Item s)))
    items' gsyms done c = do
        let itemSets = S.toList c
        gotos <- S.fromList <$> sequence [goto is x | is <- itemSets, x <- gsyms]
        let c'    = gotos S.\\ (S.insert S.empty done)
            done' = done `S.union` c
        case S.null c' of
            True  -> return done
            False -> items' gsyms done' c'

grammarSymbols :: Ord s => Grammar s (Set (Any (Atom s)))
grammarSymbols = do
    rs <- gets rules
    let ts   = S.unions
             $ concatMap (\(Any (Rule ss)) -> map grammarSymbolsS ss) (M.elems rs)
        nts  = S.fromList
             $ map (\(Any i) -> Any (ARule i)) (M.keys rs)
    return $ ts `S.union` nts
  where
    grammarSymbolsS :: Ord s => Seq s a -> Set (Any (Atom s))
    grammarSymbolsS s = case s of
        SOne a    -> grammarSymbolsA a
        a :~: ss  -> grammarSymbolsA a `S.union` grammarSymbolsS ss
        SFun _ ss -> grammarSymbolsS ss
    grammarSymbolsA :: Ord s => Atom s a -> Set (Any (Atom s))
    grammarSymbolsA a = case a of
        ATerminal (TSymbol _) -> S.singleton (Any a)
        _                     -> S.empty

closureR :: RId s a      -> Grammar s (Set (Item s))
closureR i = closure (S.singleton $ Item (Any i) 0 0)

closure :: Set (Item s) -> Grammar s (Set (Item s))
closure its = closure' S.empty its
  where
    closure' done is = do
        let is' = is S.\\ done
        case S.null is' of
            True -> return done
            False -> do
                let done' = done `S.union` is'
                is'' <- S.unions <$> mapM closItem (S.toList is')
                closure' done' is''

    closItem i = do
        a <- nextAtom i
        case a of
            Any (ARule r) -> firstItems r
            _             -> return S.empty

    firstItems :: RId s a -> Grammar s (Set (Item s))
    firstItems i = do
        Rule ss <- getRule i
        let si = take (length ss) [0..]
        return $ S.fromList $ map (\n -> Item (Any i) n 0) si

goto :: Eq s => Set (Item s) -> Any (Atom s) -> Grammar s (Set (Item s))
goto si at = do
    let its = S.toList si
    g <- catMaybes <$> mapM (nextTest at) its
    closure $ S.fromList g
  where
    nextTest :: Eq s => Any (Atom s) -> Item s -> Grammar s (Maybe (Item s))
    nextTest x i = do
        a <- nextAtom i
        if x == a
            then return $ Just i {itPos = itPos i + 1}
            else return Nothing

nextAtom :: Item s -> Grammar s (Any (Atom s))
nextAtom (Item (Any r) prod pos) = do
    Rule ss <- getRule r
    return $ findNextS (ss !! prod) pos
  where
    findNextS :: Seq s a -> Int -> Any (Atom s)
    findNextS (ATerminal TEmpty :~: ss) n = findNextS ss n
    findNextS (SFun _ ss)               n = findNextS ss n
    findNextS (SOne a)                  0 = Any a
    findNextS (a :~: _)                 0 = Any a
    findNextS (SOne _)                  _ = Any $ ATerminal TEmpty
    findNextS (_ :~: ss)                n = findNextS ss (n - 1)

data Action
    = Shift  Int
    | Reduce Int
    | Accept
    | Error

type ActionTable s = Map Int (Map s Action)
type GotoTable   s = Map Int (Map (Any (RId s)) Int)

slr :: (Ord s, Show s)
    => Grammar s (RId s a)
    -> Grammar s (ActionTable s, GotoTable s)
slr g = do
    g' <- augment g
    c  <- flip zip [0..] <$> S.toList <$> items g'
    let cm = M.fromList c
    undefined
  where
    state :: (Int, Set (Item s))
          -> Map (Set (Item s)) Int
          -> Grammar s (Int, [s, Action])
    state (i, s) m = do
        let is = S.toList s
        
      where
        aux i = do
            a <- nextAtom i
            case a of
                ATerminal TEmpty
                ATerminal a -> do
                    g <- goto i (Any a)
                    let j = M.lookup g m
                    return $ shift j
                _ -> Nothing
                    
first :: Ord s => RId s a -> Grammar s (Set (Terminal s))
first i = do
  r <- getRule i
  firstR r

firstR :: Ord s => Rule s a -> Grammar s (Set (Terminal s))
firstR (Rule ss) = S.unions <$> mapM firstS ss

firstS :: Ord s => Seq s a -> Grammar s (Set (Terminal s))
firstS s = case s of
    SOne a    -> firstA a
    SFun _ sf -> firstS sf
    a :~: ss  -> do
        fas <- firstA a
        let fas' = S.delete TEmpty fas
        if S.member TEmpty fas
            then S.union fas' <$> firstS ss
            else return  fas'

firstA :: Ord s => Atom s a -> Grammar s (Set (Terminal s))
firstA a = case a of
    ARule r -> do
        Rule ss <- getRule r
        S.unions <$> mapM firstS ss
    ATerminal t -> return $ S.singleton t

follow :: Ord s => RId s a -> Grammar s (Set (Terminal s))
follow i = do
    rs <- getRules
    S.unions <$> mapM (followR (Any i)) rs

followR :: Ord s => Any (RId s) -> Any (IdRule s) -> Grammar s (Set (Terminal s))
followR i (Any (IdRule ri (Rule ss))) = do
    b <- or <$> mapM (`endsIn` i) ss
    rest <- if not (Any ri == i) && b then follow ri else return S.empty
    S.union rest <$> S.unions <$> mapM (followS i) ss

endsIn :: Ord s => Seq s a -> Any (RId s) -> Grammar s Bool
endsIn s i = case s of
    SOne (ARule r) | i == Any r -> return True
    SOne _    -> return False
    SFun _ sf -> endsIn sf i
    ARule r :~: ss | i == Any r -> do
        fas <- firstS ss
        if S.member TEmpty fas
            then return True
            else endsIn ss i
    _ :~: ss -> endsIn ss i

followS :: Ord s => Any (RId s) -> Seq s b -> Grammar s (Set (Terminal s))
followS i s = case s of
    SOne _    -> return S.empty
    SFun _ sf -> followS i sf
    ARule r :~: ss | i == Any r -> liftM2 S.union (followS i ss)
                                              (S.delete TEmpty <$> firstS ss)
    _ :~: ss  -> followS i ss


