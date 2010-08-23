Grempa
Olle Fredriksson 2010

The extension enabling recursive do notation.
> {-# LANGUAGE DoRec #-}

> module README where

* Example 1
A parser which accepts a single character

First import the Grempa grammar combinators
> import Data.Parser.Grempa.Grammar

The type of the grammar tells us that it operates on (lists of) characters and
will return a character as a result.

> ex1 :: GRId Char Char

Grammars are conveniently written in recursive do notation, which is not
strictly necessary for this simple grammar, but will help later on.

> ex1 = do
>   rec

Here x is the name of a new rule in this grammar.
Rules consist of lists of productions. The x rule only has one production:

>     x <- [id <@> 'a']

Every grammar needs an entry rule - somewhere to start. This is achieved by the
following:

>   return x
