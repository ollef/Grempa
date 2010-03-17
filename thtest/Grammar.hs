{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}

module Grammar where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer
import Unsafe.Coerce

--------------------------------------------------------------------------
-- Interface

($::=) :: ToRule p s a => (a -> b) -> p -> Grammar s (RId s b)
f $::= p = mkRule $ f $: p

(-|-) :: (ToRule p s a, ToRule q s a) => p -> q -> Rule s a
p -|- q = toRule p :|: toRule q

(~~) :: (ToRule p s a, ToRule q s b) => p -> q -> Rule s (a,  b)
p ~~ q = toRule p :+: toRule q

rule :: RId s a -> Rule s a
rule = Rule

symbol :: s -> Rule s s
symbol = Symbol

infixl 3 :|:
infixl 3 -|-
infixl 4 $:
infixl 5 :+:
infixl 5 ~~
infixl 2 $::=

($:) :: ToRule p s a => (a -> b) -> p -> Rule s b
f $: p = Fun f (toRule p)

mkRule :: Rule s a -> Grammar s (RId s a)
mkRule p = do
    v <- newName
    addRule v p
    return v

class ToRule b s a | b -> s a where
    toRule :: b -> Rule s a
instance ToRule Char Char Char where toRule = symbol
instance ToRule a a a => ToRule [a] a [a] where
    toRule []     = error "toRule: empty list"
    toRule [c]    = (:[]) $: toRule c 
    toRule (c:cs) = (\(x, xs) -> x : xs) $: toRule c ~~ toRule cs
instance ToRule (Rule s a) s a where toRule = id
instance ToRule (RId s a) s a where toRule = rule

--------------------------------------------------------------------------
-- Grammar rules 

data Rule s a where
  Symbol :: s -> Rule s s
  --Empty  :: P s ()
  (:|:)  :: Rule s a -> Rule s a -> Rule s a
  (:+:)  :: Rule s a -> Rule s b -> Rule s (a, b)
  Fun    :: (a -> b) -> Rule s a -> Rule s b
  Rule   :: RId  s a -> Rule s a

instance Show s => Show (Rule s a) where
  show p = case p of
      Symbol s -> show s
      --Empty    -> "e"
      p :|: q  -> show p ++ " | " ++ show q
      p :+: q  -> show p ++ " "   ++ show q 
      Fun f p  -> show p
      Rule id  -> "RULE(" ++ show id ++ ")"

--------------------------------------------------------------------------
-- Grammar

data RId s a = RId Integer
  deriving (Show)

class Equals a b where
  (=?=) :: a -> b -> Bool

instance Equals (RId s a) (RId s b) where
  RId x =?= RId y = x == y

data Binding s where
  Binding :: RId s p -> Rule s p -> Binding s

instance Show s => Show (Binding s) where
  show (Binding id p) = show id ++ " ::= " ++ show p

data Env s = Env
  { bindings :: [Binding s]
  , names    :: [Integer]
  }

defaultEnv :: Env s
defaultEnv = Env {bindings = [], names = [1..]}

newtype Grammar s a = Grammar 
    { unGrammar 
      :: StateT (Env s)
                Identity
                a
    } deriving ( Monad, MonadFix, Functor
               , MonadState (Env s)
               )

execGrammar :: Grammar s a -> [Binding s]
execGrammar = bindings
            . runIdentity
            . flip execStateT defaultEnv
            . unGrammar

evalGrammar :: Grammar s a -> a
evalGrammar = runIdentity
            . flip evalStateT defaultEnv
            . unGrammar

addRule :: RId s p -> Rule s p -> Grammar s ()
addRule v p = do
    st <- get
    put st {bindings = Binding v p : bindings st}

newName :: Grammar s (RId s p)
newName = do
    st <- get
    let v:vs = names st
    put st {names = vs}
    return $ RId v

getRule :: RId s p -> Grammar s (Rule s p)
getRule x = findRule x <$> gets bindings
  where
    findRule :: RId s p -> [Binding s] -> Rule s p
    findRule x (Binding x' p : bs) | x =?= x'  = unsafeCoerce p -- Hack!
                                   | otherwise = findRule x bs

--------------------------------------------------------------------------
-- Any rule

data AnyR s where
    AnyR :: Rule s a -> AnyR s 

instance Show s => Show (AnyR s) where
    show (AnyR p) = show p
