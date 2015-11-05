{- | Grammar construction combinators.

    A grammar in grempa consists of a number of rules and an entry rule.
    Constructing a grammar is similar to doing it in BNF, but the grammars
    also have the information of what semantic action to take when a production
    has been found, which is used by the parsers that can be generated from the
    grammars.

    Rules, constructed with the 'rule' function, consist of lists of productions.

    A production in Grempa starts with a function which acts as the semantic
    action to be taken when that production has been parsed. After the '<@>'
    operator follows what the production accepts, which consists of a number of
    grammar symbols (terminals (tokens) or non-terminals (grammar rules)).

    The two combinator functions that construct productions come in two flavours
    each: One that signals that the result from parsing the symbol to the right
    of it should be used in the semantic action function and one that signals
    that it should not:

    @action '<@>' symbol =@ An action function followed by a symbol

    @action '<@'  symbol =@ An action function followed by a symbol which will
                            not be used when taking the semantic action of the
                            production.

    @prod   '<#>' symbol = @A production followed by a symbol

    @prod   '<#'  symbol = @A production followed by a symbol which will not be
                            used when taking the semantic action of the
                            production.
    The grammars have the type @'Grammar' t a@, which tells us that the grammar
    describes a language operating on @[t]@ returning @a@.

    Grammars can be recursively defined by using recursive do-notation.
-}

{-# LANGUAGE RecursiveDo, TypeFamilies #-}
module Data.Parser.Grempa.Grammar
    ( module Data.Parser.Grempa.Grammar.Typed
    , module Data.Parser.Grempa.Grammar.Levels
    , several0, several, severalInter0, severalInter, cons
    ) where

import Data.Typeable
import Data.Parser.Grempa.Grammar.Typed
    (Grammar, rule, Symbol(..), ToSym(..), (<#>), (<#), (<@>), (<@), epsilon)
import Data.Parser.Grempa.Grammar.Levels

-- | Create a new rule which consists of 0 or more of the argument symbol.
--   Example: @several0 x@ matches @x x ... x@
--
--   Creates one new rule.
several0 :: (ToSym s x, ToSymT s x ~ a, Typeable a, Typeable s)
        => x -> Grammar s [a]
several0 x = do
  rec
    xs <- rule [epsilon []
               ,(:) <@> x <#> xs]
  return xs

-- | Return a new rule which consists of 1 or more of the argument symbol.
--   Example: @several x@ matches @x x ... x@
--
--   Creates one new rule.
several :: (ToSym s x, ToSymT s x ~ a, Typeable a, Typeable s)
        => x -> Grammar s [a]
several x = do
    rec
      xs <- rule [(:[]) <@> x
                 ,(:)   <@> x <#> xs ]
    return xs

-- | Create a new rule which consists of a list of size 0 or more interspersed
--   with a symbol.
--   Example: @severalInter0 ';' x@ matches @x ';' x ';' ... ';' x@
--   If @x :: a@ then the result is of type @[a]@.
--
--   Creates two new rules.
severalInter0 :: ( ToSym s x, ToSymT s x ~ a
                 , ToSym s t, ToSymT s t ~ s
                 , Typeable a, Typeable s)
             => t -> x -> Grammar s [a]
severalInter0 tok x = do
  rec
    xs  <- rule [(:[]) <@> x
                ,(:)   <@> x <# tok <#> xs]
    res <- rule [epsilon []
                ,id <@> xs]
  return res

-- | Return a new rule which consists of a list of size 1 or more interspersed
--   with a symbol.
--   Example: @severalInter ';' x@ matches @x ';' x ';' ... ';' x@
--
--   Creates one new rule.
severalInter :: ( ToSym s x, ToSymT s x ~ a
                , ToSym s t, ToSymT s t ~ s
                , Typeable a, Typeable s)
             => t -> x -> Grammar s [a]
severalInter tok x = do
  rec
    xs <- rule [ (:[]) <@> x
               , (:)   <@> x <# tok <#> xs]
  return xs

-- | Takes two symbols and combines them with @(:)@.
--
--   Creates one new rule.
--
--   This can for example be used instead of using both 'several' and 'several0'
--   on the same symbol, as that will create three new rules, whereas the
--   equivalent using 'cons' will only create two new rules. Example
--   transformation:
--
-- > xs0 <- several0 x
-- > xs  <- several  x
-- >   ==>
-- > xs0 <- several0 x
-- > xs  <- x `cons` xs0
cons :: ( ToSym s x,  ToSymT s x   ~ a
        , ToSym s xs, ToSymT s xs ~ [a]
        , Typeable a, Typeable s)
        => x  -- ^ Symbol of type @a@
        -> xs -- ^ Symbol of type @[a]@
        -> Grammar s [a]
cons x xs = rule [(:) <@> x <#> xs]
