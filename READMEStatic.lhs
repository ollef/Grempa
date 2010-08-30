The Template Haskell extension is required to construct parsers at compile time.

> {-# LANGUAGE TemplateHaskell #-}

> module READMEStatic where

> import Data.Parser.Grempa.Static

This is where the grammars are

> import README


* Example 1

For making static parsers, Grempa needs both the "representation" of the grammar,
which in this case is achieved by [|ex1|], and the grammar itself, which is the
reason for the repetition of arguments to the function.

> parseEx1Static = $(mkStaticParser [|ex1|] ex1)

> runParseEx1Static = map parseEx1Static ex1inputs
