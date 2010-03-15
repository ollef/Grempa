{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}

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

infixl 5 :|:
infixl 5 |||
infixl 6 :+:
infixl 6 +++
infixl 3 $::=

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

newtype Grammar s a = Grammar 
    { unGrammar 
      :: StateT (Env s)
                Identity
                a
    } deriving ( Monad, MonadFix, Functor
               , MonadState (Env s)
               )

runGrammar :: Grammar s a -> [Binding s]
runGrammar = bindings
           . runIdentity
           . flip execStateT defaultEnv
           . unGrammar
  where
    defaultEnv = Env {bindings = [], names = [1..]}

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

leftMostG :: GId s a -> P s b -> Grammar s (AnyP s)
leftMostG x p = do
    let l = leftMost p
    case l of
        AnyP (Rule y) -> if x =?= y then return l else do
            p <- getRule y
            leftMostG x p
        _ -> return l

{-
data FromP s a = forall p. ToP p s a => FromP (p s a)

fromP :: P s a -> FromP s a
fromP (Symbol s) = FromP $ PSymbol s
fromP (p :|: q)  = FromP $ PChoice (fromP p) (fromP q)
fromP (p :+: q)  = FromP $ PSeq    (fromP p) (fromP q)
fromP (F f p)    = FromP $ PFun f  (fromP p)

data PSymbol s a where 
    PSymbol :: s -> PSymbol s s
data PChoice p q s a where 
    PChoice :: p s a -> q s a -> PChoice p q s a
data PSeq p q s x where
    PSeq :: p s a -> q s b -> PSeq p q s (a, b)
data PFun f p s x where 
    PFun :: (a -> b) -> p s a -> PFun (a -> b) p s b
data PRule s a where 
    PRule :: GId s a -> PRule s a

instance Show s => Show (PSymbol s s) where
    show (PSymbol s) = show s
instance (Show (p s a), Show (q s a)) => Show (PChoice p q s a) where
    show (PChoice p q) = show p ++ " | " ++ show q
instance (Show (p s a), Show (q s b)) => Show (PSeq p q s (a, b)) where
    show (PSeq p q) = show p ++ " " ++ show q
instance (Show (p s a)) => Show (PFun (a -> b) p s b) where
    show (PFun f p) = show p
instance Show (PRule s a) where
    show (PRule i) = "RULE(" ++ show i ++ ")"

class ToP r s a where
    toP :: r s a -> P s a
instance ToP PSymbol s a where
    toP (PSymbol s) = Symbol s
instance (ToP p s a, ToP q s a) => ToP (PChoice p q) s a where
    toP (PChoice p q) = toP p :|: toP q
instance (ToP p s a, ToP q s b) => ToP (PSeq p q) s (a, b) where
    toP (PSeq p q) = toP p :+: toP q
instance ToP p s a => ToP (PFun (a -> b) p) s b where
    toP (PFun f p) = F f (toP p)
instance ToP PRule s a where
    toP (PRule i) = Rule i
instance ToP FromP s a where
    toP (FromP p) = toP p

class LeftMost p s a q b | p s a -> q b where
    leftMost :: p s a -> q s b
instance LeftMost PSymbol s a PSymbol a where
    leftMost p = p
instance LeftMost p s a q b => LeftMost (PChoice p p') s a q b where
    leftMost (PChoice p q) = leftMost p
instance LeftMost p s a q b => LeftMost (PSeq p q) s (a, b) q b where
    leftMost (PSeq p q) = leftMost p
instance LeftMost p s a q c => LeftMost (PFun (a -> b) p) s b q c where
    leftMost (PFun f p) = leftMost p
instance LeftMost PRule s a PRule a where
    leftMost p = p
instance LeftMost FromP s a p b where
    leftMost (FromP p) = leftMost p
    
    -}

--instance LeftMost p s 
{-
elim :: GId s p -> Grammar s ()
elim x = do
    p <- getRule x
    --case leftMost p of
        --F f p
    return ()

class LeftMost a b | a -> b where
  leftMost :: P s a -> P s b

instance LeftMost (a, b) a where
  leftMost (p :+: _) = p

instance LeftMost a a where
  leftMost (p :|: _)    = p
  leftMost p@(Symbol _) = p
  leftMost p@(Rule _)   = p
  --leftMost _ = undefined


--instance LeftMost a b

  Symbol :: s -> P s s
  (:|:)  :: P s a -> P s a -> P s a
  (:+:)  :: P s a -> P s b -> P s (a, b)
  F      :: (a -> b) -> P s a -> P s b
  Rule   :: GId s a -> P s a
  -}
