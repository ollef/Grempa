{-# LANGUAGE DoRec #-}
module Data.Parser.Grempa.Grammar.Levels
    ( levels
    , lrule
    ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Fix
import Data.Typeable

import Data.Parser.Grempa.Grammar.Typed

newtype RStateT s m a = RStateT { runRStateT :: s -> m (a, s) }

instance MonadFix m => Monad (RStateT s m) where
    return x = RStateT $ return . (,) x
    RStateT p >>= f = RStateT $ \s0 -> do
      rec
        (x, s2) <- p s1
        (y, s1) <- runRStateT (f x) s0
      return (y, s2)

instance MonadTrans (RStateT s) where
    lift x = RStateT $ \s -> do
        r <- x
        return (r, s)

instance MonadFix m => MonadFix (RStateT s m) where
    mfix f = RStateT $ \s2 -> do
      rec
        (x, s0) <- runRStateT (f y) s1
        (y, s1) <- runRStateT (f x) s2
      return (x, s0)

get :: Monad m => RStateT s m s
get = RStateT $ \s -> return (s, s)

put :: Monad m => s -> RStateT s m ()
put s = RStateT $ const $ return ((), s)

type Levels s r m a = RStateT (Maybe (RId s r)) m a

levels :: Monad m => RStateT (Maybe a) m r -> m r
levels l = do
    (a, s) <- runRStateT l Nothing
    return a

lrule r = do
  rec
    rid <- lift $ rule $ case mnext of
        Just next -> (id <@> next) : r
        Nothing   -> r
    put (Just rid)
    mnext <- get
  return rid

