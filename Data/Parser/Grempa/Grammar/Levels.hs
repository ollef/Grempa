{-# LANGUAGE DoRec #-}
module Data.Parser.Grempa.Grammar.Levels
    ( levels
    , lrule
    ) where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Fix
import Data.Typeable

import Data.Parser.Grempa.Grammar.Typed


newtype ReverseT m a = ReverseT { runReverseT :: m a }

instance MonadFix m => Monad (ReverseT m) where
    return            = ReverseT . return
    ReverseT m >>= f  =
         ReverseT $ do
           rec
             b <- runReverseT (f a)
             a <- m
           return b

instance MonadTrans ReverseT where
    lift = ReverseT

instance MonadFix m => MonadFix (ReverseT m) where
    mfix f = ReverseT $ mfix (runReverseT . f)

type RStateT s m a = ReverseT (StateT s m) a

levels :: Monad m => RStateT (Maybe a) m r -> m r
levels = flip evalStateT Nothing . runReverseT


lrule :: (Typeable a, Typeable t)
      => Rule t a
      -> RStateT (Maybe (RId t a)) (GrammarState t) (RId t a)
lrule r = do
  rec
    lift $ put (Just rid)
    rid <- lift $ lift $ rule $ case mnext of
        Just next -> (id <@> next) : r
        Nothing   -> r
    mnext <- lift get
  return rid

