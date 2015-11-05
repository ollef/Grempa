{-# LANGUAGE FlexibleContexts, RecursiveDo, UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, TypeSynonymInstances #-}
module Data.Parser.Grempa.Grammar.Levels
    ( levels
    , lrule
    ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Fix
import Data.Typeable

import Data.Parser.Grempa.Grammar.Typed

newtype ReverseT m a = ReverseT { runReverseT :: m a }

instance (MonadFix m, Applicative (ReverseT m)) => Monad (ReverseT m) where
    return            = ReverseT . return
    ReverseT m >>= f  =
         ReverseT $ do
           rec
             b <- runReverseT (f a)
             a <- m
           return b

instance MonadTrans ReverseT where
    lift = ReverseT

instance (MonadFix m, Applicative (ReverseT m)) => MonadFix (ReverseT m) where
    mfix f = ReverseT $ mfix (runReverseT . f)

type RStateT s m = ReverseT (StateT s m)

-- | Start a levels block. Usage:
--
-- > expr <- levels $ do
-- >   rec
-- >     e <- lrule [ Plus  <@> e <# '+' <#> t ]
-- >     t <- lrule [ Times <@> t <# '*' <#> f ]
-- >     f <- lrule [ Var   <@ 'x'
-- >                , id    <@ '(' <#> e <# ')']
-- >   return e
--
-- is equivalent to
--
-- > e <- rule [ Plus  <@> e <# '+' <#> t 
-- >           , id    <@> t
-- >           ]
-- > t <- rule [ Times <@> t <# '*' <#> f 
-- >           , id    <@> f
-- >           ]
-- > f <- rule [ Var   <@ 'x'
-- >           , id    <@ '(' <#> e <# ')'
-- >           ]
--
-- Put simply, every lrule save for the last one gets an additional identity
-- production pointing to the next lrule. This is a common pattern when
-- creating grammars with precedence levels.
levels :: Monad m => RStateT (Maybe a) m r -> m r
levels = flip evalStateT Nothing . runReverseT

deriving instance Functor (RStateT (Maybe (RId t a)) (GrammarState t))
deriving instance Applicative (RStateT (Maybe (RId t a)) (GrammarState t))

-- | A rule in a levels block
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

