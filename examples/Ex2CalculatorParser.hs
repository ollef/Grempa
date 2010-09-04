-- | Generate parsers for the calculator.

-- Needed for generating parsers at compile-time.
{-# LANGUAGE TemplateHaskell #-}
module Ex2CalculatorParser where

-- Normally you would only import one of these depending on whether you want
-- to generate parsers at compile-time or runtime, but here we will show both.
import Data.Parser.Grempa.Static
import Data.Parser.Grempa.Dynamic

-- Import the grammar
import Ex2Calculator

-- Now we can use 'mkStaticParser' just like before
parseCalcStatic :: Parser CToken Integer
parseCalcStatic = $(mkStaticParser calc [|calc|])

parseCalcStaticResult :: [CToken] -> Integer
parseCalcStaticResult = parse parseCalcStatic

-- When dealing with dynamic parsers, 'ToPat' cannot be used, and we instead
-- have to wrap the tokens into something that has the desired properties.
-- Here we are wrapping them in 'constrWrapper' which will have the same result
-- as using 'toConstrPat' when making a static parser.
parseCalcDynamic :: Parser CToken Integer
parseCalcDynamic = mkDynamicParser constrWrapper calc

parseCalcDynamicResult :: [CToken] -> Integer
parseCalcDynamicResult = parse parseCalcDynamic

-- | Try a parser out on some input token strings.
--   Run it using for example @'test' 'parseCalcStaticResult'.
--   Notice that we get the 'Integer' result directly.
test :: ([CToken] -> Integer) -> [Integer]
test p = map p inputStrings
  where
    inputStrings =
      [ [Num 2, Plus, Num 3]
      , [Num 2, Times, Num 3, Plus, Num 4, Times, Num 5]
      , [Num 2, Times, LParen, Num 3, Plus, Num 4, RParen, Times, Num 5]
      , [Num 2, Times, LParen, LParen, LParen, LParen, Num 3
                     , RParen, RParen, RParen, RParen]
      ]
