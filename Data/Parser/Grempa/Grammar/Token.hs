-- | The token datatypes used internally in the parser generators.
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, UndecidableInstances, FlexibleInstances #-}
module Data.Parser.Grempa.Grammar.Token
    ( Tok(..)
    , tokToString
    , ETok(..)
    , Token
    ) where

import Data.Typeable
import Data.Data
import Language.Haskell.TH.Lift

-- | A Tok is either a token or 'EOF'.
data Tok t  = Tok {unTok :: t}
            | EOF
  deriving (Eq, Ord, Show, Data, Typeable)

$(deriveLift ''Tok)

instance Functor Tok where
    fmap f (Tok s) = Tok (f s)
    fmap _ EOF     = EOF

-- | Show the token in a more readable way. Used for error messages.
tokToString :: Show s => Tok s -> String
tokToString (Tok s)  = show s
tokToString EOF      = "EOF"

-- Data type for token or epsilon (empty).
data ETok s = ETok {unETok :: s}
            | Epsilon
  deriving (Eq, Ord, Show)

instance Functor ETok where
    fmap f (ETok s) = ETok (f s)
    fmap _ Epsilon  = Epsilon

-- | Shorthand class for instances of Data, Ord and Show.
class (Data s, Ord s, Show s) => Token s where
instance (Data s, Ord s, Show s) => Token s where
