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

rule :: GId s a -> P s a
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

instance Equals (GId s a) (GId s b) where
  GId x =?= GId y = x == y

data Binding s where
  Binding :: GId s p -> P s p -> Binding s

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

infixr 1 -|
infixr 5 :~:
infixr 5 -~
infixr 2 -$
infixr 0 -=

(-~) :: (To Atom s a p, To Seq s b q) => p -> q -> Seq s (Pair a b)
p -~ q = to p :~: to q

(-$) :: To Seq s a p => (a -> b) -> p -> Seq s b
f -$ p = SFun f (to p)

(-|) :: (To Seq s a p, To Rule s a q) => p -> q -> Rule s a
p -| q = Rule (to p : rs)
  where (Rule rs) = to q

(-=) :: (Show s, To Rule s a p) => (a -> b) -> p -> Grammar s (RId s b)
f -= r = addRule $ Rule $ map (SFun f) rs
  where (Rule rs) = to r

sym   :: s -> Atom s s
sym   = ATerminal . TSymbol
empty :: Atom s s
empty = ATerminal   TEmpty
rule  :: RId s a -> Atom s a
rule  = ARule

class To c s a t | c t -> s a where
    to :: t -> c s a

instance To Rule s a (Rule s a) where
    to = id
instance To Rule s a (Seq s a) where
    to = Rule . (:[])
instance To Rule s a (Atom s a) where
    to = to . SOne
instance To Rule s a (RId s a) where
    to = to . ARule
--instance To Rule s s s where
    --to = to . symbol

instance To Seq s a (Seq s a) where
    to = id
instance To Seq s a (RId s a) where
    to = SOne . to
instance To Seq s a (Atom s a) where
    to = SOne . to
--instance To Seq s s s where
    --to = to . symbol

instance To Atom s a (RId s a) where
    to = ARule
instance To Atom s a (Atom s a) where
    to = id
--instance To Atom s s s where
    --to = symbol

---------------------------------------------------------------------
data Rule s a = Rule
    { unRule :: [Seq s a] }
  deriving Show

infixr 5 :~
data Pair a b = a :~ b

data Seq s a where
    (:~:) :: Atom s a -> Seq s b -> Seq s (Pair a b)
    SOne  :: Atom s a -> Seq s a
    SFun  :: (a -> b) -> Seq s a -> Seq s b

data Atom s a where
    ATerminal :: Terminal s -> Atom s s
    ARule     :: RId s a    -> Atom s a

data Terminal s where
    TEmpty  ::      Terminal s
    TSymbol :: s -> Terminal s
  deriving (Ord, Eq)

data Any t where
    Any :: t a -> Any t

newtype RId s a = RId Id
  deriving Show
type    Id      = Int

data GrammarState s = GrammarState
    { rules  :: Map (Any (RId s)) (Any (Rule s))
    , ids    :: [Id]
    }

type Grammar s a = State (GrammarState s) a

data IdRule s a = IdRule (RId s a) (Rule s a)
  deriving Show

---------------------------------------------------------------------

instance Show s => Show (Seq s a) where
    show (a :~: as) = show a ++ " " ++ show as
    show (SOne r)   = show r
    show (SFun _ r) = show r

instance Show s => Show (Atom s a) where
    show (ATerminal t) = show t
    show (ARule _)   = "RULE(_)"

instance Show s => Show (Terminal s) where
    show TEmpty      = "ε"
    show (TSymbol s) = show s

instance Eq (Any (RId s)) where
    Any (RId i) == Any (RId j) = i == j
instance Ord (Any (RId s)) where
    Any (RId i) `compare` Any (RId j) = i `compare` j
instance Show (Any (RId s)) where
    show (Any (RId i)) = show i

instance Eq s => Eq (Any (Atom s)) where
    Any (ATerminal x) == Any (ATerminal y) = x == y
    Any (ARule x)     == Any (ARule y)     = Any x == Any y
    _                 == _                 = False
instance Ord s => Ord (Any (Atom s)) where
    Any (ATerminal x) `compare` Any (ATerminal y) = x `compare` y
    Any (ARule x)     `compare` Any (ARule y)     = Any x `compare` Any y
    Any (ATerminal _) `compare` _                 = LT
    Any (ARule _)     `compare` _                 = GT
instance Show s => Show (Any (Atom s)) where
    show (Any (ATerminal x)) = show x
    show (Any (ARule x))     = show x

data Item s = Item
    { itRule :: Any (RId s)
    , itProd :: Int
    , itPos  :: Int
    }
  deriving (Show, Eq, Ord)

---------------------------------------------------------------------

nextId :: Grammar s Id
nextId = do
    st <- get
    let i = head $ ids st
    put st {ids = tail (ids st)}
    return i

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

