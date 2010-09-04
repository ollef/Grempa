-- | Generate parsers for the simple expression grammar.

-- Needed for generating parsers at compile-time.
{-# LANGUAGE TemplateHaskell #-}
module Ex1SimpleExprParser where

-- Normally you would only import one of these depending on whether you want
-- to generate parsers at compile-time or runtime, but here we will show both.
import Data.Parser.Grempa.Static
import Data.Parser.Grempa.Dynamic

-- Import the grammar
import Ex1SimpleExpr

-- | The type of this function tells us that it is a parser for a
--   language operating on lists of 'Char's returning an 'E' if the parsing
--   is successful.
parseExprStatic :: Parser Char E
-- For making static parsers, Grempa needs both the "representation" of the
-- grammar, which in this case is achieved by [|expr|], and the grammar itself,
-- which is the reason for the repetition of arguments to the function.
parseExprStatic = $(mkStaticParser expr [|expr|])

-- | @'Parser' t a@ is a synynom to @[t] -> 'Either' ('ParseError' t) a@.
--   Often the desired functionality is @[t] -> a@ where the parser will throw
--   an exception if something goes wrong. The 'parse' function does just that
--   transformation to the parser.
parseExprStaticResult :: String -> E
parseExprStaticResult = parse parseExprStatic

-- | For making dynamic parsers, no Template Haskell magic is needed.
--   A parser will be created at runtime, which can take some time for big
--   grammars, but it makes it possible to create grammars that for example
--   depend on some input.
--
--   The function mkDynamicParser takes as a first argument a tuple consisting
--   of a wrap and an unwrap function to be run on all input tokens before and
--   after parsing respectively. This can sometimes be useful when the Eq and
--   Ord instances of the token type are not what is desired in the parser, as
--   we will see in later examples.
--
--   For this grammar, we will use the idWrapper (=(id, id)) which does not wrap
--   the tokens.
parseExprDynamic :: Parser Char E
parseExprDynamic = mkDynamicParser idWrapper expr

-- | You can do the same transformation as before to the dynamically generated
--   parsers.
parseExprDynamicResult :: String -> E
parseExprDynamicResult = parse parseExprDynamic

-- | Try a parser out on some input strings.
--   Run it using for example @test 'parseExprStaticResult'.
--   Notice that the precedence levels are what we are normally used to
--   and that the parentheses are included in the result not by a separate
--   constructor, but just by the structure.
test :: (String -> E) -> [E]
test p = map p inputStrings
  where
    inputStrings =
      [ "x+x"
      , "x*x+x*x"
      , "x*(x+x)*x"
      , "x*((((x))))"
      ]
