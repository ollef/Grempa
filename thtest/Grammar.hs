{-# LANGUAGE GADTs, RankNTypes, GeneralizedNewtypeDeriving, ExistentialQuantification #-}
module Grammar
  ( Grammar, runGrammar
  , (+++), (|||)
  , symbol, rule
  , mkRule
  , (->::=)
  ) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

data P s a where
  Symbol :: s -> P s s
  (:|:)  :: P s a -> P s a -> P s a
  (:+:)  :: P s a -> P s b -> P s (a, b)
  F      :: (a -> b) -> P s a -> P s b
  Rule   :: GIdent s a -> P s a

instance Show s => Show (P s a) where
  show p = case p of
      Symbol s -> show s
      p :|: q  -> show p ++ " | " ++ show q
      p :+: q  -> show p ++ " "   ++ show q 
      F f p    -> show p
      Rule id  -> "REF(" ++ show id ++ ")"

infixl 3 :+:
infixl 2 :|:
infixl 3 +++
infixl 2 |||
infixl 1 ->::=

(->::=) :: (a -> b) -> P s a -> Grammar s (GIdent s b)
f ->::= p = mkRule (F f p)

(|||) = (:|:)
(+++) = (:+:)
rule = Rule
symbol = Symbol

data GIdent s a = GIdent Integer
  deriving Show

data Binding s = forall p. Binding (GIdent s p) (P s p)
--data PolyId s  = forall p. PolyId  (GIdent s p)

newtype Grammar s a = Grammar 
    { unGrammar 
      :: WriterT [Binding s] 
       ( StateT  [Integer] 
         Identity
       ) a
    } deriving ( Monad, MonadFix
               , MonadWriter [Binding s]
               , MonadState  [Integer]
               )

defaultIds :: [Integer]
defaultIds = [1..]

runGrammar :: Grammar s a -> [Binding s]
runGrammar = runIdentity
           . flip evalStateT defaultIds
           . execWriterT
           . unGrammar

mkRule :: P s p -> Grammar s (GIdent s p)
mkRule p = do
    v <- newVar
    tell [Binding v p]
    return v

newVar :: Grammar s (GIdent s x)
newVar = do
    v:vs <- get
    put vs
    return $ GIdent v
