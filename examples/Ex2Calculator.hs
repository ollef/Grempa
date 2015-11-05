-- | Example 2: Parsing a list of tokens instead of a 'String' and computing 
--              the desired result directly.
--              In this example it is assumed that there exists a lexer
--              that goes from @'String' -> 'CToken'@, so that an input
--              'String' can be fed into the lexer and then into the generated
--              parser.

-- Needed for recursive do notation.
{-# LANGUAGE RecursiveDo #-}
-- Needed for deriving 'Typeable'.
{-# LANGUAGE DeriveDataTypeable #-}
-- Needed for deriving 'Lift'.
{-# LANGUAGE TemplateHaskell #-}
-- Needed for deriving ToSym CToken CToken
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Ex2Calculator where

-- First import the Grempa grammar combinators.
import Data.Parser.Grempa.Grammar
-- We also need the 'ToPat' class to be in scope.
import Data.Parser.Grempa.Static (ToPat(..), toConstrPat)

-- The result datatype must be an instance of the 'Typeable' typeclass.
-- Fortunately, it is possible to derive an instance. Using the extension
-- above.
import Data.Typeable
import Data.Data
-- For deriving 'Lift' instances.
import Language.Haskell.TH.Lift

-- Our token datatype. The parser will operate on a list of those.
data CToken
    = Num {unNum :: Integer}
    | Plus
    | Times
    | LParen | RParen
  -- Tokens have to have instances of a number of typeclasses ('Data', 'Eq',
  -- 'Ord' and 'Show'). When making a static parser, they also have to be
  -- members of 'Typeable' and also 'Lift' for 'toConstrPat' to work.
  deriving (Data, Eq, Ord, Show, Typeable)

instance ToSym CToken CToken where
  type ToSymT CToken CToken = CToken
  toSym = STerm

-- Derive a 'Lift' instance
$(deriveLift ''CToken)

-- The tokens of the language we are making a static parser for must have a
-- 'ToPat' instance, which provides a way for Grempa to convert the token
-- to a Template Haskell pattern matching. For tokens that should only be
-- compared on the constructor level, the implementation is easy, as there is
-- a function to do just that in Grempa.
instance ToPat CToken where
    toPat = toConstrPat

-- | Our grammar operates on lists of 'CTokens' and returns the 'Integer'
-- result directly, without computing a tree-shaped result.
calc :: Grammar CToken Integer
-- This is very similar to the definition of the previous example, but using
-- operators operating on 'Integer's instead of constructors for the semantic
-- actions.
-- Here we are using 'levels' and 'lrule's which means that the rules will
-- be linked together automatically with identity rules.
calc = levels $ do
  rec
    e  <- lrule [ (+)   <@> e <# Plus  <#> t ]
    t  <- lrule [ (*)   <@> t <# Times <#> f ]
    f  <- lrule [ id    <@  LParen <#> e <# RParen
                , unNum <@> num
                ]
  return e
  where
    -- We are using the fact that the parser will be able to only look at the
    -- constructors when comparing different tokens if we want it to work that
    -- way, which is why we can use for example this to represent any number
    -- token.
    num = Num 0

