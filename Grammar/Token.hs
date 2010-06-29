{-# LANGUAGE DeriveDataTypeable, UndecidableInstances, FlexibleInstances #-}
module Token where

import Data.Typeable
import Data.Data
import Data.Function

data Tok s  = Tok {unTok :: s}
            | RightEnd
  deriving (Eq, Ord, Show)

-- Data type for token or epsilon
data ETok s = ETok {unETok :: s}
            | Epsilon
  deriving (Eq, Ord, Show)

instance Functor Tok where
    fmap f (Tok s)  = Tok (f s)
    fmap f RightEnd = RightEnd

instance Functor ETok where
    fmap f (ETok s) = ETok (f s)
    fmap f Epsilon  = Epsilon

class (Data s, Ord s, Show s) => Token s where
instance (Data s, Ord s, Show s) => Token s where

-- | Type for representing tokens only caring about the constructor
--  TODO: Refactor this somewhere
data CTok a = CTok {unCTok :: a}
  deriving (Show, Data, Typeable)

instance Token a => Eq (CTok a) where
    CTok x == CTok y = ((==) `on` toConstr) x y

instance (Show a, Data a, Ord a) => Ord (CTok a) where
    CTok x `compare` CTok y = case ((==) `on` toConstr) x y of
        True  -> EQ
        False -> x `compare` y
