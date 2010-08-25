{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, UndecidableInstances, FlexibleInstances #-}
module Data.Parser.Grempa.Grammar.Token where

import Data.Typeable
import Data.Data
import Data.Function
import Language.Haskell.TH.Lift

data Tok s  = Tok s
            | RightEnd
  deriving (Eq, Ord, Show, Data, Typeable)

unTok :: Tok s -> s
unTok (Tok s) = s
unTok _       = error "unTok"

tokToString :: Show s => Tok s -> String
tokToString (Tok s)  = show s
tokToString RightEnd = "EOF"

$(deriveLift ''Tok)

-- Data type for token or epsilon
data ETok s = ETok {unETok :: s}
            | Epsilon
  deriving (Eq, Ord, Show)

instance Functor Tok where
    fmap f (Tok s)  = Tok (f s)
    fmap _ RightEnd = RightEnd

instance Functor ETok where
    fmap f (ETok s) = ETok (f s)
    fmap _ Epsilon  = Epsilon

class (Data s, Ord s, Show s) => Token s where
instance (Data s, Ord s, Show s) => Token s where

-- | Type for representing tokens only caring about the constructor
--  TODO: Refactor this somewhere
data CTok a = CTok {unCTok :: a}
  deriving (Show, Data, Typeable)

instance Token a => Eq (CTok a) where
    CTok x == CTok y = ((==) `on` toConstr) x y

instance Token a => Ord (CTok a) where
    CTok x `compare` CTok y = case ((==) `on` toConstr) x y of
        True  -> EQ
        False -> x `compare` y
