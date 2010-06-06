{-# LANGUAGE GADTs, DeriveDataTypeable, UndecidableInstances, FlexibleInstances #-}
module Token where

import Data.Typeable
import Data.Data
import Data.Function

class (Data s, Ord s, Show s) => Token s where
instance (Data s, Ord s, Show s) => Token s where

-- | Type for representing tokens only caring about the constructor
data CTok a where
    CTok :: {unCTok :: a} -> CTok a
  deriving (Show, Data, Typeable)

instance Token a => Eq (CTok a) where
    CTok x == CTok y = ((==) `on` toConstr) x y

instance (Show a, Data a, Ord a) => Ord (CTok a) where
    CTok x `compare` CTok y = case ((==) `on` toConstr) x y of
        True  -> EQ
        False -> x `compare` y
