{-# LANGUAGE PackageImports, GADTs, GeneralizedNewtypeDeriving, ExistentialQuantification, RankNTypes, KindSignatures #-}
module Grammar where
  {-( Grammar, runGrammar
  , (+++), (|||)
  , symbol, rule
  , mkRule
  , ($::=)
  ) where -}

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer

--------------------------------------------------------------------------
-- Interface

{-($::=) :: (a -> b) -> P s a -> Grammar s (GIdent s b)
f $::= p = mkRule $ f <$> p

(|||) :: P s a -> P s a -> P s a
(|||) = (:|:)

(+++) :: P s a -> P s b -> P s (a,  b)
(+++) = (:+:)

rule :: GIdent s a -> P s a
rule = Rule

symbol :: s -> P s s
symbol = Symbol

infixl 5 :|:
infixl 5 |||
infixl 6 :+:
infixl 6 +++
infixl 3 $::=

instance Functor (P s) where
  fmap = F
-}
--------------------------------------------------------------------------
-- Grammar rules (Parser)
data P s a where
  Symbol :: s -> P s s
  (:|:)  :: P s a -> P s a -> P s a
  (:+:)  :: P s a -> P s b -> P s (a, b)
  F      :: (a -> b) -> P s a -> P s b
  Rule   :: Ref a env -> P s a

instance Show s => Show (P s a) where
  show p = case p of
      Symbol s -> show s
      p :|: q  -> show p ++ " | " ++ show q
      p :+: q  -> show p ++ " "   ++ show q 
      F f p    -> show p
      Rule r  -> "RULE()"

data Ref a env where
    Zero :: Ref a (a, env')
    Succ :: Ref a env' -> Ref a (x, env')

data Env f env where
    Empty :: Env f ()
    Ext   :: f a -> Env f env' -> Env f (a, env')

lookupRef :: Ref a env -> Env f env -> f a
lookupRef Zero     (Ext p _ ) = p
lookupRef (Succ r) (Ext _ ps) = lookupRef r ps

updateRef :: Ref a env -> (f a -> f a) -> Env f env -> Env f env
updateRef Zero     f (Ext p ps) = Ext (f p) ps
updateRef (Succ r) f (Ext p ps) = Ext p (updateRef r f ps)

data GEnv s env  = forall a. GEnv (Env (P s) env) (Ref a env)

{-newtype Grammar s a = Grammar { unGrammar :: StateT (GEnv s) Identity a } 
  deriving 
    ( Functor, Monad, MonadFix
    , MonadState (GEnv s)
    ) -}

mkRule :: GEnv s env -> P s a -> GEnv s (a, env')
mkRule (GEnv env refs) p = GEnv (Ext p env) (Succ refs)

--------------------------------------------------------------------------
-- Grammar
{-
data GIdent s a = GIdent Integer
  deriving (Show, Eq)

data Binding s = forall p. Binding (GIdent s p) (P s p)

data Bindings s = Bindings
  { bindings :: [Binding s]
  , names    :: [Integer]
  }

instance Show s => Show (Binding s) where
  show (Binding id p) = show id ++ " ::= " ++ show p


newtype Grammar s a = Grammar 
    { unGrammar :: StateT (Bindings s) Identity a
    } deriving (Functor, Monad, MonadFix, MonadState (Bindings s))

runGrammar :: Grammar s a -> [Binding s]
runGrammar = bindings
           . runIdentity
           . flip execStateT defaultBindings
           . unGrammar
  where
    defaultBindings = Bindings [] [1..]

newVar :: Grammar s (GIdent s x)
newVar = do
    st <- get
    let v:vs = names st
    put st {names = vs}
    return $ GIdent v

addRule :: GIdent s p -> P s p -> Grammar s ()
addRule i p = do
    st <- get
    put st {bindings = Binding i p : bindings st}

mkRule :: P s p -> Grammar s (GIdent s p)
mkRule p = do
    v <- newVar
    addRule v p
    return v

findRule :: GIdent s p -> Grammar s (P s p)
findRule i = find i <$> gets bindings

find :: GIdent s p -> [Binding s] -> P s p
find i (Binding i' p : bs) 
    | i == i'   = p
    | otherwise = find i bs
    
--------------------------------------------------------------------------
-- Left recursion elimination

elim :: GIdent s p -> Grammar s ()
elim i = do
    r <- findRule i
    return ()
-}

