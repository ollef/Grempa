{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}


module Grammar2 where

import Control.Applicative
import Control.Monad.ST
import Data.STRef
import Unsafe.Coerce

--infixl 3 :|:
--infixl 3 |-
infixl 5 :~:
--infixl 5 ~~
--infixr 0 &&

data One
data Succ a

data Func parity a b where
    SF :: (a -> b) -> Func One a b
    DF :: ((a, b) -> c) -> Func depth c d -> Func (Succ depth) (a, b) d

class FuncToFun a b | a -> b where
    funcToFun :: a -> b

instance FuncToFun (Func One a b) (a -> b) where
    funcToFun (SF f) = f

instance FuncToFun (Func depth c d) (c -> d) 
      => FuncToFun (Func (Succ depth) (a, b) d) ((a, b) -> d)
      where
    funcToFun (DF f df) (a, b) = (funcToFun df (a, b)) --funcToFun df (f a) 
    

class Uncurry d p r | d p -> r where
    uncur :: p -> r

--instance Uncurry depth (bc -> d)    ((bc'  -> d), One)
      -- => Uncurry (Succ depth) (a  -> bc -> d) ((a, bc') -> d) where
    --uncur f (a, bc) = uncur (f a) bc

instance Uncurry One (a -> b) (a -> b) where
    uncur = id

--instance Uncurry depth             (bc -> d)     (bc'  -> d)
      -- => Uncurry (Succ depth) (a -> bc -> d) ((a, bc') -> d) where
   --uncur f (a, bc) = uncur (f a) bc 

data Rule s a where
    RuleChoices :: [RSeq d s a] -> Rule s a
--data Rule s a where

    --(:|:)  :: RSeq d s a -> Rule s a -> Rule s a
    --Single :: RSeq d s a -> Rule s a
--instance Show s => Show (Rule s a) where
    --show (a :|: b) = show a ++ " | " ++ show b

data RSeq d s a where
    (:~:) :: RAtom s a -> RSeq d s b -> RSeq (Succ d) s (a, b)
    ROne  :: RAtom s a -> RSeq One s a
    Fun   :: (a -> b) -> RSeq d s a -> RSeq One s b

instance Show s => Show (RSeq d s a) where
    show (a :~: as) = show a ++ " " ++ show as
    show (ROne r)   = show r
    show (Fun f r)  = show r

data RAtom s a where
    Symbol :: s         -> RAtom s s
    Rule   :: RId t s a -> RAtom s a
instance Show s => Show (RAtom s a) where
    show (Symbol s) = show s
    show (Rule _)   = "RULE()"

data AnyRule s where
    AnyRule :: Rule s a -> AnyRule s

data RId t s a = RId (STRef t (AnyRule s))

addRule :: Rule s a -> ST t (RId t s a) 
addRule r = RId <$> newSTRef (AnyRule r)

getRule :: RId t s a -> ST t (Rule s a)
getRule (RId ref) = do
    (AnyRule rule) <- readSTRef ref
    return $ unsafeCoerce rule

evalGrammar = runST
