-- | Example 1: Parsing simple expressions of the form @"x*(x+x)+x"@ with the
--              correct precedence levels.

-- Needed for recursive do notation.
{-# LANGUAGE RecursiveDo #-}
-- Needed for deriving 'Typeable'.
{-# LANGUAGE DeriveDataTypeable #-}
-- Needed for deriving ToSym Char Char
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Ex1SimpleExpr where

-- First import the Grempa grammar combinators.
import Data.Parser.Grempa.Grammar
-- The result datatype must be an instance of the 'Typeable' typeclass.
-- Fortunately, it is possible to derive an instance. Using the extension
-- above.
import Data.Typeable

-- | The result data structure.
data E = Plus  E E
       | Times E E
       | Var
  deriving (Show, Eq, Typeable)

instance ToSym Char Char where
    type ToSymT Char Char = Char
    toSym = STerm

-- | The type of the 'expr' function tells us that it is a grammar for a
--   language operating on lists of 'Char's returning an 'E' if the parsing
--   is successful.
expr :: Grammar Char E
expr = do
  -- Recursive do notation is used so that a rule defined before another rule
  -- can still use that other rule. This is not strictly necessary for all
  -- grammars, but for this one, it is.
  rec
    -- Here @e@ will be the name of a new rule in the grammar (@e@ for 
    -- expression).
    -- The semantic action to take when @e@ has been found is to build a result
    -- of type 'E' using the 'Plus' constructor. Since we're using '<#' before
    -- the '+', it means that the result from parsing that will not be applied
    -- to the 'Plus' constructor.
    e <- rule [ Plus  <@> e <# '+' <#> t
              -- An @e@ can also be a @t@ (term, defined below) and then we just
              -- want to return that result, because @t@ will also have results
              -- of type @E@. So just use the identity function.
              , id    <@> t
              ]
    -- Similar to @e@ but with the multiplication sign instead, using the
    -- 'Times' constructor to construct the result.
    t <- rule [ Times <@> t <# '*' <#> f
              -- A @t@ can also be an @f@ (factor).
              , id    <@> f
              ]
    -- An @f@ can either be an expression in parentheses, or a variable
    -- (written 'x' in the language). Notice the use of '<@' and '<#' when not
    -- using a symbol when constructing the result of the production.
    f <- rule [ id  <@ '(' <#> e <# ')'
              , Var <@ 'x'
              ]
  -- Lastly, we need to return the entry rule of the grammar.
  return e
