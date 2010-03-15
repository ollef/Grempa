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

($::=) :: (a -> b) -> P s a -> Grammar s (GId s b)
f $::= p = mkRule $ f <$> p

(|||) :: P s a -> P s a -> P s a
(|||) = (:|:)

(+++) :: P s a -> P s b -> P s (a,  b)
(+++) = (:+:)

rule :: GId s a -> P s a
rule = Rule

symbol :: s -> P s s
symbol = Symbol

infixl 3 :|:
infixl 3 |||
infixl 5 :+:
infixl 5 +++
infixl 2 $::=

test = symbol 'a' +++ symbol 'b' ||| symbol 'c' +++ symbol 'd' 

instance Functor (P s) where
  fmap = F

mkRule :: P s p -> Grammar s (GId s p)
mkRule p = do
    v <- newName
    addRule v p
    return v

--------------------------------------------------------------------------
-- Grammar rules (Parser)

data P s a where
  Symbol :: s -> P s s
  (:|:)  :: P s a -> P s a -> P s a
  (:+:)  :: P s a -> P s b -> P s (a, b)
  F      :: (a -> b) -> P s a -> P s b
  Rule   :: GId s a -> P s a

instance Show s => Show (P s a) where
  show p = case p of
      Symbol s -> show s
      p :|: q  -> show p ++ " | " ++ show q
      p :+: q  -> show p ++ " "   ++ show q 
      F f p    -> show p
      Rule id  -> "RULE(" ++ show id ++ ")"

--------------------------------------------------------------------------
-- Grammar

data GId s a = GId Integer
  deriving (Show)

class Equals a b where
  (=?=) :: a -> b -> Bool

instance Equals (GId s a) (GId s b) where
  GId x =?= GId y = x == y

data Binding s where
  Binding :: GId s p -> P s p -> Binding s

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

newVar :: Grammar s (GId s x)
newVar = do
    st <- get
    let v:vs = names st
    put st {names = vs}
    return $ GId v

addRule :: GId s p -> P s p -> Grammar s ()
addRule v p = do
    st <- get
    put st {bindings = Binding v p : bindings st}

newName :: Grammar s (GId s p)
newName = do
    st <- get
    let v:vs = names st
    put st {names = vs}
    return $ GId v

getRule :: GId s p -> Grammar s (P s p)
getRule x = findRule x <$> gets bindings
  where
    findRule :: GId s p -> [Binding s] -> P s p
    findRule x (Binding x' p : bs) | x =?= x'  = unsafeCoerce p -- Hack!
                                   | otherwise = findRule x bs

--------------------------------------------------------------------------

data AnyP s where
    AnyP :: P s a -> AnyP s 

instance Show s => Show (AnyP s) where
    show (AnyP p) = show p

leftMost :: P s a -> AnyP s
leftMost p = case p of
    p :|: _ -> leftMost p
    p :+: _ -> leftMost p
    F f p   -> leftMost p
    _       -> AnyP p

leftMostG :: GId s a -> Grammar s (AnyP s)
leftMostG x = do
    p <- getRule x
    leftMostG' x p
  where
    leftMostG' :: GId s a -> P s b -> Grammar s (AnyP s)
    leftMostG' x p = do
        let l = leftMost p
        case l of
            AnyP (Rule y) -> if x =?= y then return l else do
                p <- getRule y
                leftMostG' x p
            _ -> return l

leftRec :: GId s a -> Grammar s ()
leftRec = undefined

-- A = baaaaaaaaaa
-- A  ::= A a | b
-- =>
-- A  ::= b A'
-- A' ::= e | a A';
