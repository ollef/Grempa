Grempa
Olle Fredriksson 2010

The extension enabling recursive do notation.

> {-# LANGUAGE DoRec #-}

> module README where

* Grammars
* Example 1 - A parser which will only accept the string "ab" or "ba" and
  return the first character of the parsed string.

The corresponding BNF grammar for this parser would be the following:
x ::= 'a''b'
    | 'b''a'

First import the Grempa grammar combinators.

> import Data.Parser.Grempa.Grammar

The type of this grammar tells us that it operates on (lists of) characters and
will return a single character as a result.
Note that a type signature is usually required for a grammar to typecheck.

> ex1 :: GRId Char Char

Grammars are conveniently written in recursive do notation, which is not
strictly necessary for this simple grammar, but will help later on.

> ex1 = do
>   rec

Here x will be the name of a new rule in this grammar.

>     x <- rule

Rules, constructed with the rule function, consist of lists of productions.

A production in Grempa starts with a function which acts as the semantic
action to be taken when that production has been parsed. After the <@> operator
follows what the production accepts, which consists of a number of grammar
symbols (terminals (tokens) or non-terminals (grammar rules)).

The two combinator functions that construct productions come in two flavours
each: One that signals that the result from parsing the symbol to the right of
it should be used in the semantic action function and one that signals that it
should not:

action <@> symbol = An action function followed by a symbol
action <@  symbol = An action function followed by a symbol which will not be used
                    when taking the semantic action of the production.
prod   <#> symbol = A production followed by a symbol
prod   <#  symbol = A production followed by a symbol which will not be used
                    when taking the semantic action of the production.

By marking only the first characters to be used when constructing the result
and using the id function, a parser which returns the first character in the
parsed string is achieved:

>         [ id <@> 'a' <# 'b'
>         , id <@> 'b' <# 'a'
>         ]

Every grammar needs an entry rule - somewhere to start. This is achieved by the
following:

>   return x

[INSERT CODE FOR MAKING DYNAMIC PARSER]
See READMEStatic.lhs for the static version of this (which will be constructed
at compile time).
